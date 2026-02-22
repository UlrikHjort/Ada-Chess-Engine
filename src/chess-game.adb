-- ***************************************************************************
--                      Chess - Game Module Body
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

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Exceptions; use Ada.Exceptions;
with Chess.Board; use Chess.Board;
with Chess.Display; use Chess.Display;
with Chess.Hash; use Chess.Hash;
with Chess.Moves; use Chess.Moves;
with Chess.Notation; use Chess.Notation;
with Chess.PGN; use Chess.PGN;
with Chess.Tactics; use Chess.Tactics;
with Chess.Eval;    use Chess.Eval;
with Chess.Search;  use Chess.Search;

package body Chess.Game is

   procedure Show_Help is
   begin
      Put_Line ("Commands:");
      Put_Line ("  <move>       - Make a move (e.g., e4, Nf3, O-O, exd5)");
      Put_Line ("  moves        - Show all legal moves");
      Put_Line ("  undo         - Undo last move");
      Put_Line ("  redo         - Redo move");
      Put_Line ("  save <file>  - Save game to PGN file");
      Put_Line ("  load <file>  - Load game from PGN file");
      Put_Line ("  analyze <N>  - Find mate in N moves (1-4)");
      Put_Line ("  eval         - Evaluate current position");
      Put_Line ("  go [v] [depth]   - Engine plays best move (v = verbose)");
      Put_Line ("  go time <ms>     - Engine plays using iterative deepening within time budget");
      Put_Line ("  autoplay [depth] - Engine plays both sides (default depth 3)");
      Put_Line ("  help         - Show this help");
      Put_Line ("  quit         - Exit game");
      New_Line;
   end Show_Help;

   procedure Show_Legal_Moves (State : Game_State) is
      Legal : constant Move_List := Generate_All_Legal_Moves (State, State.Current_Player);
      Count : Natural := 0;
   begin
      Put_Line ("Legal moves:");
      for Move of Legal loop
         Put (Move_To_Algebraic (State, Move) & " ");
         Count := Count + 1;
         if Count mod 8 = 0 then
            New_Line;
         end if;
      end loop;
      if Count mod 8 /= 0 then
         New_Line;
      end if;
      Put_Line ("Total:" & Natural'Image (Count) & " moves");
      New_Line;
   end Show_Legal_Moves;

   -- Build a position hash history array from the session history for
   -- repetition detection inside the search.
   procedure Build_Hash_History (Session       :     Game_Session;
                                 History       : out Hash_Array;
                                 History_Count : out Natural) is
   begin
      History_Count := 0;
      for I in 0 .. Session.History_Index loop
         exit when History_Count >= Max_Game_History;
         History (History_Count) :=
           Compute_Hash (Session.History.Element (I));
         History_Count := History_Count + 1;
      end loop;
   end Build_Hash_History;

   procedure Start_Game_Loop is
      Session : Game_Session;
      Input : String (1 .. 100);
      Last : Natural;
      Move : Move_Type;
   begin
      Put_Line ("Ada Chess Engine v0.1");
      Put_Line ("====================");
      New_Line;
      Put_Line ("Type 'help' for commands");
      New_Line;
      
      -- Initialize game
      Session.Current := Initial_Position;
      Session.History.Append (Session.Current);
      Session.History_Index := 0;
      
      loop
         Show_Game_Info (Session.Current);
         Show_Board (Session.Current, Use_Unicode => False);
         
         -- Check for game end
         if Is_Checkmate (Session.Current, Session.Current.Current_Player) then
            Put_Line ("╔════════════════════════════════════╗");
            Put_Line ("║          CHECKMATE!                ║");
            Put ("║  ");
            if Session.Current.Current_Player = White then
               Put_Line ("Black wins!                    ║");
            else
               Put_Line ("White wins!                    ║");
            end if;
            Put_Line ("╚════════════════════════════════════╝");
            exit;
         elsif Is_Stalemate (Session.Current, Session.Current.Current_Player) then
            Put_Line ("╔════════════════════════════════════╗");
            Put_Line ("║          STALEMATE!                ║");
            Put_Line ("║          Draw!                     ║");
            Put_Line ("╚════════════════════════════════════╝");
            exit;
         elsif Session.Current.Halfmove_Clock >= 100 then
            Put_Line ("╔════════════════════════════════════╗");
            Put_Line ("║       50-MOVE RULE!                ║");
            Put_Line ("║          Draw!                     ║");
            Put_Line ("╚════════════════════════════════════╝");
            exit;
         end if;
         
         -- Check
         if Is_In_Check (Session.Current, Session.Current.Current_Player) then
            Put_Line (">>> CHECK! <<<");
            New_Line;
         end if;
         
         -- Get user input
         Put (if Session.Current.Current_Player = White then "White" else "Black");
         Put (" to move> ");
         Get_Line (Input, Last);
         
         if Last = 0 then
            -- Empty input
            null;
         else
            declare
               Command : constant String := Trim (Input (1 .. Last), Ada.Strings.Both);
            begin
               if Command = "quit" or Command = "exit" or Command = "q" then
                  Put_Line ("Thanks for playing!");
                  exit;
                  
               elsif Command = "help" or Command = "h" or Command = "?" then
                  Show_Help;
                  
               elsif Command = "moves" or Command = "m" then
                  Show_Legal_Moves (Session.Current);
                  
               elsif Command = "undo" or Command = "u" then
                  if Session.History_Index > 0 then
                     Session.History_Index := Session.History_Index - 1;
                     Session.Current := Session.History.Element (Session.History_Index);
                     Put_Line ("Move undone.");
                     New_Line;
                  else
                     Put_Line ("Nothing to undo.");
                     New_Line;
                  end if;
                  
               elsif Command = "redo" or Command = "r" then
                  if Session.History_Index < Natural (Session.History.Length) - 1 then
                     Session.History_Index := Session.History_Index + 1;
                     Session.Current := Session.History.Element (Session.History_Index);
                     Put_Line ("Move redone.");
                     New_Line;
                  else
                     Put_Line ("Nothing to redo.");
                     New_Line;
                  end if;
                  
               elsif Command'Length > 5 and then Command (1 .. 5) = "save " then
                  declare
                     Filename : constant String := Trim (Command (6 .. Command'Last), Ada.Strings.Both);
                     PGN_Data : PGN_Game;
                  begin
                     PGN_Data.Initial_State := Initial_Position;
                     
                     -- Build move history
                     for I in 1 .. Session.History_Index loop
                        declare
                           Prev_State : constant Game_State := Session.History.Element (I - 1);
                           Curr_State : constant Game_State := Session.History.Element (I);
                           Legal : constant Move_List := Generate_All_Legal_Moves (Prev_State, Prev_State.Current_Player);
                        begin
                           -- Find the move that was made
                           for Move of Legal loop
                              if Apply_Move (Prev_State, Move).Board = Curr_State.Board then
                                 PGN_Data.Move_History.Append (Move);
                                 exit;
                              end if;
                           end loop;
                        end;
                     end loop;
                     
                     -- Set metadata
                     PGN_Data.Event (1 .. 13) := "Casual Game  ";
                     PGN_Data.Event_Last := 13;
                     PGN_Data.Site (1 .. 1) := "?";
                     PGN_Data.Site_Last := 1;
                     PGN_Data.Date (1 .. 10) := "????.??.??";
                     PGN_Data.Date_Last := 10;
                     PGN_Data.Round (1 .. 1) := "?";
                     PGN_Data.Round_Last := 1;
                     PGN_Data.White (1 .. 6) := "Player";
                     PGN_Data.White_Last := 6;
                     PGN_Data.Black (1 .. 6) := "Player";
                     PGN_Data.Black_Last := 6;
                     PGN_Data.Result (1 .. 1) := "*";
                     PGN_Data.Result_Last := 1;
                     
                     Save_PGN (Filename, PGN_Data);
                     Put_Line ("Game saved to " & Filename);
                     New_Line;
                  exception
                     when E : others =>
                        Put_Line ("Error saving game: " & Exception_Message (E));
                        New_Line;
                  end;
                  
               elsif Command'Length > 5 and then Command (1 .. 5) = "load " then
                  declare
                     Filename : constant String := Trim (Command (6 .. Command'Last), Ada.Strings.Both);
                     PGN_Data : PGN_Game;
                     Temp_State : Game_State;
                  begin
                     PGN_Data := Load_PGN (Filename);
                     
                     -- Clear history and rebuild from PGN
                     Session.History.Clear;
                     Session.History.Append (PGN_Data.Initial_State);
                     Temp_State := PGN_Data.Initial_State;
                     
                     for Move of PGN_Data.Move_History loop
                        Temp_State := Apply_Move (Temp_State, Move);
                        Session.History.Append (Temp_State);
                     end loop;
                     
                     Session.History_Index := Natural (Session.History.Length) - 1;
                     Session.Current := Session.History.Element (Session.History_Index);
                     
                     Put_Line ("Game loaded from " & Filename);
                     Put_Line ("Loaded" & Natural'Image (Natural (PGN_Data.Move_History.Length)) & " moves");
                     New_Line;
                  exception
                     when E : others =>
                        Put_Line ("Error loading game: " & Exception_Message (E));
                        New_Line;
                  end;
                  
               elsif Command'Length > 8 and then Command (1 .. 8) = "analyze " then
                  declare
                     N_Str : constant String := Trim (Command (9 .. Command'Last), Ada.Strings.Both);
                     N : Positive;
                  begin
                     N := Positive'Value (N_Str);
                     
                     if N > 4 then
                        Put_Line ("Analyzing mate in" & Positive'Image (N) & " may take very long.");
                        Put_Line ("Recommended: 1-4 moves");
                     end if;
                     
                     Put_Line ("Searching for mate in" & Positive'Image (N) & " moves...");
                     
                     declare
                        Solution : constant Solution_Type := Find_Mate_In_N (Session.Current, N);
                     begin
                        Show_Solution (Session.Current, Solution);
                        Print_Statistics;  -- PHASE 4: Show performance stats
                     end;
                     New_Line;
                  exception
                     when E : others =>
                        Put_Line ("Error analyzing position: " & Exception_Message (E));
                        New_Line;
                  end;

               elsif Command = "eval" then
                  Put_Line (Evaluate_Verbose (Session.Current));
                  New_Line;

               elsif Command = "go" or else
                     (Command'Length > 3 and then Command (1 .. 3) = "go ") then
                  declare
                     Depth    : Positive := Default_Depth;
                     Time_Ms  : Natural  := 0;   -- 0 = fixed depth, >0 = timed
                     Verbose  : Boolean  := False;
                     Arg      : constant String :=
                       (if Command'Length > 3 then Command (4 .. Command'Last) else "");
                     Eng_Move : Move_Type;
                     Before   : Game_State;
                     Hist     : Hash_Array;
                     H_Count  : Natural;
                  begin
                     -- Parse "go time <ms>", "go v", "go verbose", "go <N>",
                     --       "go v <N>", "go verbose <N>"
                     if Arg'Length >= 5 and then Arg (Arg'First .. Arg'First + 3) = "time" then
                        -- go time <ms>
                        declare
                           Rest : constant String :=
                             Ada.Strings.Fixed.Trim (Arg (Arg'First + 4 .. Arg'Last),
                                                     Ada.Strings.Both);
                        begin
                           begin
                              Time_Ms := Positive'Value (Rest);
                           exception
                              when others =>
                                 Put_Line ("Usage: go time <milliseconds>");
                                 New_Line;
                                 goto Continue;
                           end;
                        end;
                     else
                        declare
                           Rest : constant String :=
                             (if Arg'Length >= 8 and then Arg (Arg'First .. Arg'First + 6) = "verbose"
                              then Arg (Arg'First + 7 .. Arg'Last)
                              elsif Arg'Length >= 1 and then Arg (Arg'First) = 'v'
                              then Arg (Arg'First + 1 .. Arg'Last)
                              else Arg);
                           Had_V : constant Boolean :=
                             (Arg'Length >= 8 and then Arg (Arg'First .. Arg'First + 6) = "verbose")
                             or else (Arg'Length >= 1 and then Arg (Arg'First) = 'v');
                           Trimmed : constant String :=
                             (if Rest'Length > 0 and then Rest (Rest'First) = ' '
                              then Rest (Rest'First + 1 .. Rest'Last) else Rest);
                        begin
                           Verbose := Had_V;
                           if Trimmed'Length > 0 then
                              begin
                                 Depth := Positive'Value (Trimmed);
                              exception
                                 when others =>
                                    Put_Line ("Usage: go [v|verbose] [depth]");
                                    New_Line;
                                    goto Continue;
                              end;
                           end if;
                        end;
                     end if;

                     Build_Hash_History (Session, Hist, H_Count);
                     Before := Session.Current;

                     if Time_Ms > 0 then
                        Put_Line ("Thinking (iterative deepening," &
                                  Natural'Image (Time_Ms) & " ms)...");
                        Eng_Move := Find_Best_Move_Timed (Session.Current,
                                                          Time_Ms, Verbose,
                                                          Hist, H_Count);
                     elsif Verbose then
                        Put_Line ("Thinking at depth" & Positive'Image (Depth)
                                  & " (verbose)...");
                        Eng_Move := Find_Best_Move (Session.Current, Depth,
                                                    True, Hist, H_Count);
                     else
                        Put_Line ("Thinking at depth" & Positive'Image (Depth) & "...");
                        Eng_Move := Find_Best_Move (Session.Current, Depth,
                                                    False, Hist, H_Count);
                     end if;

                     -- Apply the engine move
                     Session.Current := Apply_Move (Session.Current, Eng_Move);

                     -- Truncate redo history
                     while Natural (Session.History.Length) > Session.History_Index + 1 loop
                        Session.History.Delete_Last;
                     end loop;

                     Session.History.Append (Session.Current);
                     Session.History_Index := Natural (Session.History.Length) - 1;

                     Put_Line ("Engine plays: " &
                       Move_To_Algebraic (Before, Eng_Move));
                     Print_Search_Stats;
                     New_Line;
                  exception
                     when No_Legal_Moves =>
                        if Is_In_Check (Session.Current, Session.Current.Current_Player) then
                           Put_Line ("Checkmate!");
                        else
                           Put_Line ("Stalemate!");
                        end if;
                        New_Line;
                  end;
                  <<Continue>>

               elsif Command = "autoplay" or else
                     (Command'Length > 9 and then Command (1 .. 9) = "autoplay ") then
                  declare
                     Depth     : Positive := 3;
                     Max_Moves : constant Natural := 100;
                     Half      : Natural  := 0;
                     Done      : Boolean  := False;
                     Before    : Game_State;
                     Eng_Move  : Move_Type;
                     Hist      : Hash_Array;
                     H_Count   : Natural;
                  begin
                     if Command'Length > 9 then
                        begin
                           Depth := Positive'Value (Command (10 .. Command'Last));
                        exception
                           when others =>
                              Put_Line ("Usage: autoplay [depth]");
                              New_Line;
                              goto After_Autoplay;
                        end;
                     end if;

                     Put_Line ("Autoplay: White vs Black, depth" &
                               Positive'Image (Depth) & " each side");
                     Put_Line (String'(1 .. 46 => '-'));

                     while not Done and then Half < Max_Moves * 2 loop
                        Half := Half + 1;
                        declare
                           Player : constant Color_Type :=
                                      Session.Current.Current_Player;
                           Moves  : constant Move_List  :=
                                      Generate_All_Legal_Moves
                                        (Session.Current, Player);
                        begin
                           if Moves.Is_Empty then
                              -- Terminal: checkmate or stalemate
                              if Half mod 2 = 0 then
                                 New_Line;  -- end the half-started line
                              end if;
                              if Is_In_Check (Session.Current, Player) then
                                 if Player = White then
                                    Put_Line ("  0-1  (Black wins by checkmate)");
                                 else
                                    Put_Line ("  1-0  (White wins by checkmate)");
                                 end if;
                              else
                                 Put_Line ("  1/2-1/2  (Stalemate)");
                              end if;
                              Done := True;

                           elsif Session.Current.Halfmove_Clock >= 100 then
                              if Half mod 2 = 0 then
                                 New_Line;
                              end if;
                              Put_Line ("  1/2-1/2  (50-move rule)");
                              Done := True;

                           else
                              Before   := Session.Current;
                              Build_Hash_History (Session, Hist, H_Count);
                              Eng_Move := Find_Best_Move (Session.Current, Depth,
                                                          False, Hist, H_Count);
                              Session.Current := Apply_Move
                                                   (Session.Current, Eng_Move);

                              declare
                                 Move_Str : constant String :=
                                   Move_To_Algebraic (Before, Eng_Move);
                                 Label : constant String :=
                                   (if Player = White
                                    then Integer'Image ((Half + 1) / 2) & ". " & Move_Str
                                    else Integer'Image ((Half + 1) / 2) & "... " & Move_Str);
                              begin
                                 Put_Line (Label);
                                 Show_Board (Session.Current, Use_Unicode => False);
                              end;
                           end if;
                        end;
                     end loop;

                     if Half >= Max_Moves * 2 and then not Done then
                        New_Line;
                        Put_Line ("  (Stopped after" & Natural'Image (Max_Moves)
                                  & " moves)");
                     end if;

                     New_Line;
                     -- Update session so user can inspect the final position
                     Session.History.Append (Session.Current);
                     Session.History_Index :=
                       Natural (Session.History.Length) - 1;
                  end;
                  <<After_Autoplay>>

               else
                  -- Try to parse as a move
                  begin
                     Move := Parse_Move (Session.Current, Command);
                     
                     -- Apply the move
                     Session.Current := Apply_Move (Session.Current, Move);
                     
                     -- Truncate history if we made a move after undo
                     while Natural (Session.History.Length) > Session.History_Index + 1 loop
                        Session.History.Delete_Last;
                     end loop;
                     
                     Session.History.Append (Session.Current);
                     Session.History_Index := Natural (Session.History.Length) - 1;
                     
                     Put_Line ("Move: " & Move_To_Algebraic (
                       Session.History.Element (Session.History_Index - 1), Move));
                     New_Line;
                     
                  exception
                     when Notation_Error =>
                        Put_Line ("Error: Invalid move notation");
                        Put_Line ("Type 'help' for commands or 'moves' to see legal moves");
                        New_Line;
                     when E : others =>
                        Put_Line ("Error: " & Exception_Name (E) & " - " & Exception_Message (E));
                        New_Line;
                  end;
               end if;
            end;
         end if;
      end loop;
   end Start_Game_Loop;

end Chess.Game;
