-- ***************************************************************************
--                      Chess - UCI Protocol Body
--
--           Copyright (C) 2026 By Ulrik Hørlyk Hjort
--
-- Permission is hereby granted, free of charge, to any person obtaining
-- a copy of this software and associated documentation files (the
-- "Software"), to deal in the Software without restriction, including
-- without limitation the rights to use, copy, modify, merge, publish,
-- distribute, sublicense, and/or sell copies of the Software, and to
-- permit persons to whom the Software is furnished to do so, subject to
-- the following conditions:
--
-- The above copyright notice and this permission notice shall be
-- included in all copies or substantial portions of the Software.
--
-- THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
-- EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
-- MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
-- NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
-- LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
-- OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
-- WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
-- ***************************************************************************

with Ada.Text_IO;       use Ada.Text_IO;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Chess.Board;       use Chess.Board;
with Chess.Hash;        use Chess.Hash;
with Chess.Moves;       use Chess.Moves;
with Chess.PGN;         use Chess.PGN;
with Chess.Search;      use Chess.Search;
with Chess.Types;       use Chess.Types;

package body Chess.UCI is

   Engine_Name    : constant String := "Ada Chess";
   Engine_Author  : constant String := "Ulrik Hoerlyk Hjort";
   Default_Move_Ms: constant Positive := 3000;  -- 3 s per move default

   -- =========================================================================
   -- Helpers
   -- =========================================================================

   -- Split a string into words (space-separated), return the Nth word (1-based)
   -- Returns "" when the Nth word does not exist.
   function Word (S : String; N : Positive) return String is
      Start : Integer := S'First;
      Count : Natural := 0;
   begin
      while Start <= S'Last loop
         -- Skip leading spaces
         while Start <= S'Last and then S (Start) = ' ' loop
            Start := Start + 1;
         end loop;
         exit when Start > S'Last;
         -- Find end of word
         declare
            Stop : Integer := Start;
         begin
            while Stop <= S'Last and then S (Stop) /= ' ' loop
               Stop := Stop + 1;
            end loop;
            Count := Count + 1;
            if Count = N then
               return S (Start .. Stop - 1);
            end if;
            Start := Stop;
         end;
      end loop;
      return "";
   end Word;

   -- Return the rest of S starting from (and including) the Nth word.
   function From_Word (S : String; N : Positive) return String is
      Start : Integer := S'First;
      Count : Natural := 0;
   begin
      while Start <= S'Last loop
         while Start <= S'Last and then S (Start) = ' ' loop
            Start := Start + 1;
         end loop;
         exit when Start > S'Last;
         declare
            Stop : Integer := Start;
         begin
            while Stop <= S'Last and then S (Stop) /= ' ' loop
               Stop := Stop + 1;
            end loop;
            Count := Count + 1;
            if Count = N then
               return S (Start .. S'Last);
            end if;
            Start := Stop;
         end;
      end loop;
      return "";
   end From_Word;

   -- Parse a long-algebraic move string such as "e2e4" or "e7e8q" and
   -- return the matching Move_Type from the legal move list.
   -- Raises Constraint_Error when no match is found.
   function Parse_UCI_Move (State : Game_State;
                            S     : String) return Move_Type
   is
      Moves : constant Move_List :=
        Generate_All_Legal_Moves (State, State.Current_Player);
   begin
      for M of Moves loop
         declare
            From_S : constant String := Square_To_String (M.From);
            To_S   : constant String := Square_To_String (M.To);
            Base   : constant String := From_S & To_S;
            Full   : constant String :=
              (if M.Flag = Promotion
               then Base & (case M.Promotion is
                              when Queen  => "q",
                              when Rook   => "r",
                              when Bishop => "b",
                              when Knight => "n",
                              when others => "")
               else Base);
         begin
            if S = Full or else S = Base then
               return M;
            end if;
         end;
      end loop;
      raise Constraint_Error with "UCI move not found: " & S;
   end Parse_UCI_Move;

   -- Render a move in UCI long-algebraic notation (e2e4, e7e8q, …)
   function Move_To_UCI (M : Move_Type) return String is
      Base : constant String :=
        Square_To_String (M.From) & Square_To_String (M.To);
   begin
      if M.Flag = Promotion then
         return Base & (case M.Promotion is
                          when Queen  => "q",
                          when Rook   => "r",
                          when Bishop => "b",
                          when Knight => "n",
                          when others => "q");
      end if;
      return Base;
   end Move_To_UCI;

   -- =========================================================================
   -- Command handlers
   -- =========================================================================

   procedure Handle_UCI is
   begin
      Put_Line ("id name " & Engine_Name);
      Put_Line ("id author " & Engine_Author);
      Put_Line ("option name Hash type spin default 64 min 1 max 512");
      Put_Line ("option name Threads type spin default 1 min 1 max 1");
      Put_Line ("option name Depth type spin default"
                & Positive'Image (Default_Depth)
                & " min 1 max"
                & Positive'Image (Max_ID_Depth));
      Put_Line ("uciok");
   end Handle_UCI;

   -- Parse "position startpos [moves m1 m2 …]"
   --    or "position fen <FEN> [moves m1 m2 …]"
   procedure Handle_Position (Line   :     String;
                              State  : out Game_State;
                              Hist   : out Hash_Array;
                              H_Count: out Natural) is
      Pos    : Integer := Line'First;
      W2     : constant String := Word (Line, 2);
   begin
      H_Count := 0;
      Hist    := (others => 0);

      if W2 = "startpos" then
         State := Initial_Position;
      elsif W2 = "fen" then
         -- FEN is words 3..n up to "moves" keyword
         declare
            Fen_Str   : constant String := From_Word (Line, 3);
            Moves_Pos : Natural := 0;
         begin
            for I in Fen_Str'Range loop
               if I + 4 <= Fen_Str'Last and then
                  Fen_Str (I .. I + 4) = "moves"
               then
                  Moves_Pos := I;
                  exit;
               end if;
            end loop;
            declare
               Fen : constant String :=
                 Trim ((if Moves_Pos > 0
                        then Fen_Str (Fen_Str'First .. Moves_Pos - 1)
                        else Fen_Str),
                       Ada.Strings.Both);
            begin
               State := FEN_To_Game_State (Fen);
            exception
               when others =>
                  Put_Line ("info string Error parsing FEN: " & Fen);
                  State := Initial_Position;
            end;
         end;
      else
         State := Initial_Position;
      end if;

      -- Record initial hash
      Hist (H_Count) := Compute_Hash (State);
      H_Count := H_Count + 1;

      -- Find "moves" token and apply each move
      Pos := Line'First;
      while Pos <= Line'Last - 4 loop
         if Line (Pos .. Pos + 4) = "moves" then
            Pos := Pos + 6;  -- skip "moves "
            while Pos <= Line'Last loop
               -- Skip spaces
               while Pos <= Line'Last and then Line (Pos) = ' ' loop
                  Pos := Pos + 1;
               end loop;
               exit when Pos > Line'Last;
               -- Extract next word
               declare
                  Stop : Integer := Pos;
               begin
                  while Stop <= Line'Last and then Line (Stop) /= ' ' loop
                     Stop := Stop + 1;
                  end loop;
                  declare
                     Move_Str : constant String := Line (Pos .. Stop - 1);
                  begin
                     if Move_Str'Length >= 4 then
                        begin
                           declare
                              M : constant Move_Type :=
                                Parse_UCI_Move (State, Move_Str);
                           begin
                              State := Apply_Move (State, M);
                              if H_Count < Max_Game_History then
                                 Hist (H_Count) := Compute_Hash (State);
                                 H_Count := H_Count + 1;
                              end if;
                           end;
                        exception
                           when others =>
                              Put_Line ("info string Unknown move: " & Move_Str);
                        end;
                     end if;
                  end;
                  Pos := Stop;
               end;
            end loop;
            exit;
         end if;
         Pos := Pos + 1;
      end loop;
   end Handle_Position;

   -- Parse "go [wtime N] [btime N] [movestogo N] [depth N] [movetime N] [infinite]"
   procedure Handle_Go (Line  : String;
                        State : Game_State;
                        Hist  : Hash_Array;
                        H_Count : Natural)
   is
      W         : Natural := 2;
      Move_Time : Natural  := Default_Move_Ms;
      Depth     : Natural  := 0;   -- 0 = use time
      Wtime     : Natural  := 0;
      Btime     : Natural  := 0;
      Movestogo : Natural  := 25;
      Infinite  : Boolean  := False;
   begin
      loop
         declare
            Token : constant String := Word (Line, W);
         begin
            exit when Token = "";
            if Token = "movetime" then
               Move_Time := Natural'Value (Word (Line, W + 1));
               W := W + 2;
            elsif Token = "depth" then
               Depth := Positive'Value (Word (Line, W + 1));
               W := W + 2;
            elsif Token = "wtime" then
               Wtime := Natural'Value (Word (Line, W + 1));
               W := W + 2;
            elsif Token = "btime" then
               Btime := Natural'Value (Word (Line, W + 1));
               W := W + 2;
            elsif Token = "movestogo" then
               Movestogo := Natural'Value (Word (Line, W + 1));
               W := W + 2;
            elsif Token = "infinite" then
               Infinite := True;
               W := W + 1;
            else
               W := W + 1;
            end if;
         end;
      end loop;

      -- Determine budget
      if Infinite then
         -- Search to maximum fixed depth
         Depth := (if Depth = 0 then Max_ID_Depth else Depth);
      elsif Depth = 0 then
         -- Time management: use side-to-move clock with movestogo
         if State.Current_Player = White and then Wtime > 0 then
            Move_Time := Wtime / Movestogo;
         elsif State.Current_Player = Black and then Btime > 0 then
            Move_Time := Btime / Movestogo;
         end if;
         -- Clamp: at least 50 ms, at most 30 s
         if Move_Time < 50   then Move_Time := 50;   end if;
         if Move_Time > 30_000 then Move_Time := 30_000; end if;
      end if;

      declare
         Best : Move_Type;
      begin
         if Depth > 0 then
            Best := Find_Best_Move (State, Depth, False, Hist, H_Count);
         else
            Best := Find_Best_Move_Timed (State, Move_Time, False, Hist, H_Count);
         end if;
         Put_Line ("bestmove " & Move_To_UCI (Best));
      exception
         when No_Legal_Moves =>
            Put_Line ("bestmove 0000");  -- null move (position is terminal)
      end;
   end Handle_Go;

   -- =========================================================================
   -- Main UCI loop
   -- =========================================================================
   procedure Run_UCI_Loop is
      Input      : String (1 .. 4096);
      Last       : Natural;
      State      : Game_State := Initial_Position;
      Hist       : Hash_Array := (others => 0);
      H_Count    : Natural    := 1;
   begin
      -- Seed history with starting position
      Hist (0) := Compute_Hash (State);

      loop
         Get_Line (Input, Last);
         declare
            Line : constant String :=
              Trim (Input (1 .. Last), Ada.Strings.Both);
            Cmd  : constant String := Word (Line, 1);
         begin
            if Cmd = "uci" then
               Handle_UCI;

            elsif Cmd = "isready" then
               Put_Line ("readyok");

            elsif Cmd = "ucinewgame" then
               State   := Initial_Position;
               H_Count := 1;
               Hist    := (others => 0);
               Hist (0) := Compute_Hash (State);

            elsif Cmd = "position" then
               Handle_Position (Line, State, Hist, H_Count);

            elsif Cmd = "go" then
               Handle_Go (Line, State, Hist, H_Count);

            elsif Cmd = "stop" then
               null;  -- single-threaded; nothing to stop

            elsif Cmd = "quit" then
               exit;

            elsif Cmd = "d" then
               -- Debug: dump current position hash (extension)
               Put_Line ("info string hash "
                         & Zobrist_Hash'Image (Compute_Hash (State)));
            end if;
         end;
      end loop;
   end Run_UCI_Loop;

end Chess.UCI;
