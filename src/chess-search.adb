-- ***************************************************************************
--                      Chess - Negamax Search Engine Body
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

with Chess.Board;      use Chess.Board;
with Chess.Moves;      use Chess.Moves;
with Chess.Eval;       use Chess.Eval;
with Chess.Notation;   use Chess.Notation;
with Ada.Text_IO;      use Ada.Text_IO;
with Ada.Real_Time;    use Ada.Real_Time;

package body Chess.Search is

   -- =========================================================================
   -- Score constants (centipawns)
   -- =========================================================================
   Infinity    : constant Integer :=  1_000_000;
   Mate_Score  : constant Integer :=    900_000;
   Max_Q_Depth : constant Natural := 8;

   -- =========================================================================
   -- Principal Variation (PV) table
   -- =========================================================================
   Max_Ply : constant := 64;
   type PV_Line    is array (0 .. Max_Ply - 1) of Move_Type;
   type PV_Matrix  is array (0 .. Max_Ply - 1) of PV_Line;
   type PV_Len_Arr is array (0 .. Max_Ply - 1) of Natural;

   PV_Table : PV_Matrix;
   PV_Len   : PV_Len_Arr;

   -- =========================================================================
   -- Transposition Table
   --
   -- 2^18 = 262 144 entries (~8 MB).  Each entry stores the Zobrist key,
   -- search depth, score, bound type, and the best move for move ordering.
   -- Three bound types:
   --   Exact       – the score is the true minimax value at that depth
   --   Lower_Bound – a beta-cutoff occurred; score is a lower bound
   --   Upper_Bound – all moves failed low; score is an upper bound
   -- =========================================================================
   TT_Size : constant := 2**18;   -- must be a power of two

   type TT_Flag is (Empty, Exact, Lower_Bound, Upper_Bound);

   type TT_Entry is record
      Key      : Zobrist_Hash := 0;
      Depth    : Natural      := 0;
      Score    : Integer      := 0;
      Flag     : TT_Flag      := Empty;
      Has_Move : Boolean      := False;
      Best     : Move_Type;
   end record;

   type TT_Array is array (0 .. TT_Size - 1) of TT_Entry;
   TT : TT_Array;

   procedure TT_Store (Hash  : Zobrist_Hash;
                       Depth : Natural;
                       Score : Integer;
                       Flag  : TT_Flag;
                       M     : Move_Type;
                       Has_M : Boolean) is
      Idx : constant Natural := Natural (Hash mod TT_Size);
   begin
      TT (Idx) := (Key      => Hash,
                   Depth    => Depth,
                   Score    => Score,
                   Flag     => Flag,
                   Has_Move => Has_M,
                   Best     => M);
   end TT_Store;

   -- =========================================================================
   -- Repetition stack
   --
   -- Holds Zobrist hashes for all positions from game start through the
   -- current search node.  We return a draw score when the same hash
   -- appears a second time in the stack (i.e. the position would occur
   -- for the third time counting the root).
   -- =========================================================================
   Max_Rep : constant := 600;
   type Rep_Array is array (0 .. Max_Rep - 1) of Zobrist_Hash;
   Rep_Stack : Rep_Array := (others => 0);
   Rep_Top   : Natural   := 0;

   function Is_Repetition (Hash : Zobrist_Hash) return Boolean is
      Count : Natural := 0;
   begin
      for I in 0 .. Rep_Top - 1 loop
         if Rep_Stack (I) = Hash then
            Count := Count + 1;
            if Count >= 2 then
               return True;
            end if;
         end if;
      end loop;
      return False;
   end Is_Repetition;

   -- =========================================================================
   -- Time management
   -- =========================================================================
   Search_Start    : Time;
   Time_Budget_Ms  : Natural  := 0;   -- 0 = no time limit
   Time_Out        : Boolean  := False;

   function Elapsed_Ms return Natural is
      Span : constant Time_Span := Clock - Search_Start;
   begin
      return Natural (To_Duration (Span) * 1000.0);
   end Elapsed_Ms;

   -- =========================================================================
   -- Search statistics
   -- =========================================================================
   Nodes_Visited    : Natural := 0;
   Beta_Cutoffs     : Natural := 0;
   Quiescence_Nodes : Natural := 0;
   TT_Hits          : Natural := 0;

   -- =========================================================================
   -- Move ordering for the playing engine
   -- =========================================================================

   -- Simple MVV-LVA (Most Valuable Victim / Least Valuable Attacker) score.
   -- Captures are tried before quiet moves; within captures we prefer
   -- taking a queen with a pawn over taking a pawn with a queen.
   function Piece_Value (Kind : Piece_Kind) return Integer is
   begin
      case Kind is
         when Pawn   => return 100;
         when Knight => return 320;
         when Bishop => return 330;
         when Rook   => return 500;
         when Queen  => return 900;
         when King   => return 20_000;
      end case;
   end Piece_Value;

   function Move_Score (State : Game_State; M : Move_Type) return Integer is
      Victim : Board_Square;
   begin
      -- Promotion: very high priority
      if M.Flag = Promotion then
         return 8_000 + Piece_Value (M.Promotion);
      end if;

      -- Capture: MVV-LVA
      Victim := Get_Piece (State, M.To);
      if Victim.Content = Occupied then
         return 6_000 + Piece_Value (Victim.Piece.Kind)
                      - Piece_Value (Get_Piece (State, M.From).Piece.Kind) / 10;
      end if;

      return 0;  -- quiet move
   end Move_Score;

   -- In-place insertion sort by descending score.
   -- TT_Best_Move is placed first (score = 10_000) when Has_TT is True.
   procedure Sort_Moves (State    : Game_State;
                         Moves    : in out Move_List;
                         TT_Best  : Move_Type  := (others => <>);
                         Has_TT   : Boolean    := False)
   is
      Best_Idx : Natural;
      Best_Score, Cur_Score : Integer;
      Tmp : Move_Type;
      N   : constant Natural := Natural (Moves.Length);

      function Score_Of (M : Move_Type) return Integer is
      begin
         if Has_TT and then M = TT_Best then
            return 10_000;   -- TT move always first
         end if;
         return Move_Score (State, M);
      end Score_Of;
   begin
      for I in 0 .. N - 2 loop
         Best_Idx   := I;
         Best_Score := Score_Of (Moves (I));
         for J in I + 1 .. N - 1 loop
            Cur_Score := Score_Of (Moves (J));
            if Cur_Score > Best_Score then
               Best_Score := Cur_Score;
               Best_Idx   := J;
            end if;
         end loop;
         if Best_Idx /= I then
            Tmp              := Moves (I);
            Moves (I)        := Moves (Best_Idx);
            Moves (Best_Idx) := Tmp;
         end if;
      end loop;
   end Sort_Moves;

   -- True when a move captures an enemy piece (normal capture or en passant)
   function Is_Capture (State : Game_State; M : Move_Type) return Boolean is
   begin
      return Get_Piece (State, M.To).Content = Occupied
             or else M.Flag = En_Passant;
   end Is_Capture;

   -- =========================================================================
   -- Quiescence search
   --
   -- Called at depth 0 instead of returning a static evaluation.
   -- Keeps searching capture moves (and promotions) until the position is
   -- "quiet" — no more forcing captures available.  This prevents the
   -- horizon effect: the engine can no longer miss a hanging piece simply
   -- because the horizon fell just before the recapture.
   --
   -- Stand-pat: we first take the static eval as a lower bound.  The
   -- assumption is that the side to move can always find *some* quiet move
   -- that is at least as good as any capture, so we never have to capture.
   -- If the stand-pat score already beats beta we prune immediately.
   -- =========================================================================
   function Quiescence (State  : Game_State;
                        Alpha  : Integer;
                        Beta   : Integer;
                        QDepth : Natural := Max_Q_Depth) return Integer
   is
      A         : Integer := Alpha;
      Sign      : constant Integer :=
                    (if State.Current_Player = White then 1 else -1);
      Stand_Pat : constant Integer := Sign * Evaluate (State);
      All_Moves : Move_List;
      Captures  : Move_List;
      Score     : Integer;
      Child     : Game_State;
   begin
      Quiescence_Nodes := Quiescence_Nodes + 1;

      -- Stand-pat pruning
      if Stand_Pat >= Beta then
         return Beta;
      end if;
      if Stand_Pat > A then
         A := Stand_Pat;
      end if;

      -- Depth limit: stop capture search if we've gone far enough
      if QDepth = 0 then
         return A;
      end if;

      -- Collect and sort capture moves only
      All_Moves := Generate_All_Legal_Moves (State, State.Current_Player);
      for M of All_Moves loop
         if Is_Capture (State, M) or else M.Flag = Promotion then
            Captures.Append (M);
         end if;
      end loop;

      if Captures.Is_Empty then
         return A;   -- quiet position — stand-pat is the answer
      end if;

      Sort_Moves (State, Captures);

      for M of Captures loop
         Child := Apply_Move (State, M);
         Score := -Quiescence (Child, -Beta, -A, QDepth - 1);

         if Score >= Beta then
            Beta_Cutoffs := Beta_Cutoffs + 1;
            return Beta;
         end if;
         if Score > A then
            A := Score;
         end if;
      end loop;

      return A;
   end Quiescence;

   -- =========================================================================
   -- Negamax alpha-beta with transposition table and draw detection.
   --
   -- Returns a score in centipawns from the perspective of the side to move.
   -- Ply  = distance from root (for mate score and PV building).
   -- Hash = Zobrist hash of State, updated incrementally.
   -- =========================================================================
   function Negamax (State : Game_State;
                     Depth : Natural;
                     Ply   : Natural;
                     Alpha : Integer;
                     Beta  : Integer;
                     Hash  : Zobrist_Hash) return Integer
   is
      A          : Integer   := Alpha;
      Orig_Alpha : constant Integer := Alpha;
      Moves      : Move_List;
      Score      : Integer;
      Child      : Game_State;
      Child_Hash : Zobrist_Hash;
      Best_M     : Move_Type;
      Has_Best   : Boolean   := False;
      TT_Idx     : constant Natural := Natural (Hash mod TT_Size);
      TT_E       : TT_Entry renames TT (TT_Idx);
      TT_Move    : Move_Type;
      Has_TT_M   : Boolean   := False;
   begin
      Nodes_Visited := Nodes_Visited + 1;
      PV_Len (Ply)  := 0;

      -- Draw by fifty-move rule
      if State.Halfmove_Clock >= 100 then
         return 0;
      end if;

      -- Draw by repetition (position would occur a third time)
      if Ply > 0 and then Is_Repetition (Hash) then
         return 0;
      end if;

      -- Transposition table probe
      if TT_E.Flag /= Empty and then TT_E.Key = Hash then
         TT_Hits := TT_Hits + 1;
         if TT_E.Has_Move then
            TT_Move  := TT_E.Best;
            Has_TT_M := True;
         end if;
         if TT_E.Depth >= Depth then
            case TT_E.Flag is
               when Exact       => return TT_E.Score;
               when Lower_Bound =>
                  if TT_E.Score >= Beta then return TT_E.Score; end if;
               when Upper_Bound =>
                  if TT_E.Score <= A    then return TT_E.Score; end if;
               when Empty => null;
            end case;
         end if;
      end if;

      -- Leaf node: drop into quiescence search
      if Depth = 0 then
         return Quiescence (State, Alpha, Beta);
      end if;

      -- Periodic time check (avoid calling Clock every node)
      if Time_Budget_Ms > 0
         and then (Nodes_Visited mod 4096 = 0)
         and then Elapsed_Ms >= Time_Budget_Ms
      then
         Time_Out := True;
         return 0;
      end if;

      Moves := Generate_All_Legal_Moves (State, State.Current_Player);

      if Moves.Is_Empty then
         if Is_In_Check (State, State.Current_Player) then
            return -(Mate_Score - Ply);
         else
            return 0;  -- stalemate
         end if;
      end if;

      Sort_Moves (State, Moves, TT_Move, Has_TT_M);

      -- Push current position hash for repetition detection
      Rep_Stack (Rep_Top) := Hash;
      Rep_Top := Rep_Top + 1;

      for M of Moves loop
         exit when Time_Out;
         Child      := Apply_Move (State, M);
         Child_Hash := Update_Hash (Hash, State, M);
         Score := -Negamax (Child, Depth - 1, Ply + 1, -Beta, -A, Child_Hash);

         if Score >= Beta then
            Beta_Cutoffs := Beta_Cutoffs + 1;
            TT_Store (Hash, Depth, Score, Lower_Bound, M, True);
            Rep_Top := Rep_Top - 1;
            return Beta;
         end if;

         if Score > A then
            A        := Score;
            Best_M   := M;
            Has_Best := True;
            PV_Table (Ply)(0) := M;
            for I in 0 .. PV_Len (Ply + 1) - 1 loop
               PV_Table (Ply)(I + 1) := PV_Table (Ply + 1)(I);
            end loop;
            PV_Len (Ply) := PV_Len (Ply + 1) + 1;
         end if;
      end loop;

      Rep_Top := Rep_Top - 1;

      if not Time_Out then
         if A <= Orig_Alpha then
            TT_Store (Hash, Depth, A, Upper_Bound, Best_M, Has_Best);
         else
            TT_Store (Hash, Depth, A, Exact, Best_M, Has_Best);
         end if;
      end if;

      return A;
   end Negamax;

   -- Format a centipawn score as e.g. "+1.25" or "-0.30"
   function Format_Score (Cp : Integer) return String is
      Abs_Cp   : constant Integer := abs Cp;
      Pawns    : constant Integer := Abs_Cp / 100;
      Fraction : constant Integer := (Abs_Cp mod 100);
      Frac_Str : constant String  :=
        (if Fraction < 10 then "0" & Integer'Image(Fraction)(2..2)
         else Integer'Image(Fraction)(2..3));
      Sign_Chr : constant Character := (if Cp >= 0 then '+' else '-');
   begin
      return Sign_Chr & Integer'Image(Pawns)(2..Integer'Image(Pawns)'Last)
             & "." & Frac_Str;
   end Format_Score;

   -- =========================================================================
   -- Internal: one-depth root search (shared by fixed-depth and ID)
   -- =========================================================================
   function Root_Search (State      : Game_State;
                         Depth      : Positive;
                         Root_Hash  : Zobrist_Hash;
                         Verbose    : Boolean) return Move_Type
   is
      Moves      : Move_List;
      Best_Move  : Move_Type;
      Best_Score : Integer := -Infinity;
      Score      : Integer;
      Child      : Game_State;
      Child_Hash : Zobrist_Hash;
      Sign       : constant Integer :=
                     (if State.Current_Player = White then 1 else -1);
   begin
      PV_Len := (others => 0);
      Moves  := Generate_All_Legal_Moves (State, State.Current_Player);

      if Moves.Is_Empty then
         raise No_Legal_Moves;
      end if;

      -- TT best move at root for ordering
      declare
         TT_Idx   : constant Natural := Natural (Root_Hash mod TT_Size);
         TT_E     : TT_Entry renames TT (TT_Idx);
         TT_Root  : Move_Type;
         Has_Root : Boolean := False;
      begin
         if TT_E.Flag /= Empty and then TT_E.Key = Root_Hash and then TT_E.Has_Move then
            TT_Root  := TT_E.Best;
            Has_Root := True;
         end if;
         Sort_Moves (State, Moves, TT_Root, Has_Root);
      end;

      Best_Move := Moves.First_Element;

      for M of Moves loop
         exit when Time_Out;
         Child      := Apply_Move (State, M);
         Child_Hash := Update_Hash (Root_Hash, State, M);
         Score := -Negamax (Child, Depth - 1, 1, -Infinity, -Best_Score, Child_Hash);

         if Verbose then
            Put ("  " & Move_To_Algebraic (State, M));
            Put (String'(1 .. 8 - Move_To_Algebraic (State, M)'Length => ' '));
            Put_Line (Format_Score (Sign * Score));
         end if;

         if Score > Best_Score then
            Best_Score := Score;
            Best_Move  := M;
            PV_Table (0)(0) := M;
            for I in 0 .. PV_Len (1) - 1 loop
               PV_Table (0)(I + 1) := PV_Table (1)(I);
            end loop;
            PV_Len (0) := PV_Len (1) + 1;
         end if;
      end loop;

      if Verbose and then not Time_Out then
         New_Line;
         Put ("Best line: ");
         declare
            S : Game_State := State;
         begin
            for I in 0 .. PV_Len (0) - 1 loop
               Put (Move_To_Algebraic (S, PV_Table (0)(I)) & " ");
               S := Apply_Move (S, PV_Table (0)(I));
            end loop;
         end;
         New_Line;
         Put_Line ("Score: " & Format_Score (Sign * Best_Score));
      end if;

      return Best_Move;
   end Root_Search;

   -- =========================================================================
   -- Public: Find_Best_Move  (fixed depth)
   -- =========================================================================
   function Find_Best_Move (State         : Game_State;
                            Depth         : Positive := Default_Depth;
                            Verbose       : Boolean  := False;
                            History       : Hash_Array := (others => 0);
                            History_Count : Natural  := 0)
                            return Move_Type
   is
      Root_Hash : constant Zobrist_Hash := Compute_Hash (State);
   begin
      Nodes_Visited    := 0;
      Beta_Cutoffs     := 0;
      Quiescence_Nodes := 0;
      TT_Hits          := 0;
      Time_Budget_Ms   := 0;
      Time_Out         := False;

      -- Seed repetition stack with game history
      Rep_Top := 0;
      for I in 0 .. History_Count - 1 loop
         Rep_Stack (Rep_Top) := History (I);
         Rep_Top := Rep_Top + 1;
      end loop;

      return Root_Search (State, Depth, Root_Hash, Verbose);
   end Find_Best_Move;

   -- =========================================================================
   -- Public: Find_Best_Move_Timed  (iterative deepening, time budget)
   -- =========================================================================
   function Find_Best_Move_Timed (State         : Game_State;
                                  Time_Ms       : Positive;
                                  Verbose       : Boolean  := False;
                                  History       : Hash_Array := (others => 0);
                                  History_Count : Natural  := 0)
                                  return Move_Type
   is
      Root_Hash  : constant Zobrist_Hash := Compute_Hash (State);
      Best       : Move_Type;
      Last_Best  : Move_Type;
      Sign       : constant Integer :=
                     (if State.Current_Player = White then 1 else -1);
   begin
      Nodes_Visited    := 0;
      Beta_Cutoffs     := 0;
      Quiescence_Nodes := 0;
      TT_Hits          := 0;
      Time_Budget_Ms   := Natural (Float (Time_Ms) * 0.9);  -- 10% safety margin
      Time_Out         := False;
      Search_Start     := Clock;

      -- Seed repetition stack with game history
      Rep_Top := 0;
      for I in 0 .. History_Count - 1 loop
         Rep_Stack (Rep_Top) := History (I);
         Rep_Top := Rep_Top + 1;
      end loop;

      -- Check there are legal moves before starting
      declare
         Moves : constant Move_List :=
            Generate_All_Legal_Moves (State, State.Current_Player);
      begin
         if Moves.Is_Empty then
            raise No_Legal_Moves;
         end if;
         Last_Best := Moves.First_Element;
      end;

      for D in 1 .. Max_ID_Depth loop
         exit when Time_Out;
         exit when Elapsed_Ms >= Time_Budget_Ms;
         if Verbose then
            Put_Line ("  depth" & Positive'Image (D) & "  elapsed"
                      & Natural'Image (Elapsed_Ms) & " ms");
         end if;
         Best := Root_Search (State, D, Root_Hash, False);
         if not Time_Out then
            Last_Best := Best;
            if Verbose then
               Put ("  depth" & Positive'Image (D) & "  score ");
               declare
                  TT_Idx : constant Natural := Natural (Root_Hash mod TT_Size);
                  TT_E   : TT_Entry renames TT (TT_Idx);
               begin
                  if TT_E.Flag = Exact and then TT_E.Key = Root_Hash then
                     Put_Line (Format_Score (Sign * TT_E.Score));
                  else
                     New_Line;
                  end if;
               end;
            end if;
         end if;
      end loop;

      return Last_Best;
   end Find_Best_Move_Timed;

   -- =========================================================================
   -- Statistics
   -- =========================================================================
   procedure Print_Search_Stats is
   begin
      New_Line;
      Put_Line ("=== Search Statistics ===");
      Put_Line ("Nodes visited:      " & Natural'Image (Nodes_Visited));
      Put_Line ("Quiescence nodes:   " & Natural'Image (Quiescence_Nodes));
      Put_Line ("Beta cutoffs:       " & Natural'Image (Beta_Cutoffs));
      Put_Line ("TT hits:            " & Natural'Image (TT_Hits));
      if Time_Budget_Ms > 0 then
         Put_Line ("Time elapsed:       " & Natural'Image (Elapsed_Ms) & " ms");
      end if;
   end Print_Search_Stats;

end Chess.Search;
