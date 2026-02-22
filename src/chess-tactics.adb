-- ***************************************************************************
--                      Chess - Tactical Analysis Body
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
with Chess.Notation; use Chess.Notation;
with Chess.Board; use Chess.Board;
with Chess.Hash; use Chess.Hash;
with Interfaces; use Interfaces;

package body Chess.Tactics is

   -- ============================================================================
   -- PHASE 1 OPTIMIZATION: Move Ordering
   -- ============================================================================
   
   -- Killer move table: Stores moves that caused cutoffs at each depth
   type Killer_Array is array (0..20, 1..2) of Move_Type;
   Killer_Moves : Killer_Array;

   -- ============================================================================
   -- HISTORY HEURISTIC: Track which moves cause cutoffs across the whole tree
   -- ============================================================================
   -- Indexed by [from_file, from_rank, to_file, to_rank]. Incremented when a
   -- move causes a beta cutoff. Used as a bonus score for quiet moves.
   subtype Board_File is Chess.Types.File_Type;
   subtype Board_Rank is Chess.Types.Rank_Type;
   type History_Table is
      array (Board_File, Board_Rank, Board_File, Board_Rank) of Natural;
   History : History_Table := (others => (others => (others => (others => 0))));

   procedure Reset_History is
   begin
      History := (others => (others => (others => (others => 0))));
   end Reset_History;

   procedure Update_History (Move : Move_Type; Depth_Left : Natural) is
      Bonus : constant Natural := Depth_Left * Depth_Left;
   begin
      History(Move.From.File, Move.From.Rank, Move.To.File, Move.To.Rank) :=
         History(Move.From.File, Move.From.Rank, Move.To.File, Move.To.Rank) + Bonus;
   end Update_History;
   
   -- ============================================================================
   -- TRANSPOSITION TABLE: Cache search results to avoid re-searching positions
   -- ============================================================================
   -- Entry stores only Mate_Found flag (not the full line) to keep memory small.
   -- Only used when Depth_Left >= TT_Min_Depth to avoid overhead for trivial nodes.
   type Hash_Entry is record
      Zobrist_Hash : Chess.Hash.Zobrist_Hash := 0;
      Depth_Left   : Natural := 0;
      Mate_Found   : Boolean := False;
      Age          : Natural := 0;
   end record;

   Table_Size    : constant := 2**18;  -- 256K entries (~10 MB)
   TT_Min_Depth  : constant := 2;      -- Only cache when >= 2 plies remain
   type Hash_Index is mod Table_Size;
   type Transposition_Table is array (Hash_Index) of Hash_Entry;

   TT          : Transposition_Table;
   Current_Age : Natural := 0;
   TT_Hits     : Natural := 0;
   TT_Misses   : Natural := 0;
   TT_Stores   : Natural := 0;

   procedure Clear_Transposition_Table is
   begin
      Current_Age := Current_Age + 1;
      TT_Hits   := 0;
      TT_Misses := 0;
      TT_Stores := 0;
   end Clear_Transposition_Table;

   function Probe_Table (
      Hash       : Zobrist_Hash;
      Depth_Left : Natural;
      Valid      : out Boolean;
      Mate_Found : out Boolean
   ) return Boolean is
      Index    : constant Hash_Index := Hash_Index(Unsigned_64(Hash) mod Table_Size);
      TT_Entry : constant Hash_Entry := TT(Index);
   begin
      if TT_Entry.Zobrist_Hash = Hash and
         TT_Entry.Depth_Left >= Depth_Left and
         TT_Entry.Age = Current_Age then
         TT_Hits    := TT_Hits + 1;
         Valid      := True;
         Mate_Found := TT_Entry.Mate_Found;
         return TT_Entry.Mate_Found;
      else
         TT_Misses := TT_Misses + 1;
         Valid      := False;
         Mate_Found := False;
         return False;
      end if;
   end Probe_Table;

   procedure Store_In_Table (
      Hash       : Zobrist_Hash;
      Depth_Left : Natural;
      Mate_Found : Boolean
   ) is
      Index : constant Hash_Index := Hash_Index(Unsigned_64(Hash) mod Table_Size);
   begin
      -- Always-replace strategy
      TT(Index) := (
         Zobrist_Hash => Hash,
         Depth_Left   => Depth_Left,
         Mate_Found   => Mate_Found,
         Age          => Current_Age
      );
      TT_Stores := TT_Stores + 1;
   end Store_In_Table;
   
   -- Initialize with null moves
   procedure Init_Killers is
      Null_Move : constant Move_Type := (
         From => (File_A, 1),
         To => (File_A, 1),
         Flag => Normal,
         Promotion => Queen
      );
   begin
      for D in Killer_Moves'Range(1) loop
         for K in Killer_Moves'Range(2) loop
            Killer_Moves(D, K) := Null_Move;
         end loop;
      end loop;
   end Init_Killers;
   
   -- Get piece value for MVV-LVA scoring
   function Piece_Value (Piece : Piece_Type) return Integer is
   begin
      case Piece.Kind is
         when Pawn   => return 100;
         when Knight => return 300;
         when Bishop => return 300;
         when Rook   => return 500;
         when Queen  => return 900;
         when King   => return 10000;
      end case;
   end Piece_Value;
   
   -- Score a move for ordering (higher = better)
   function Score_Move (
      State : Game_State;
      Move  : Move_Type;
      Depth : Natural
   ) return Integer is
      Score : Integer := 0;
      From_Square : Board_Square;
      To_Square : Board_Square;
   begin
      -- Get squares
      From_Square := Get_Piece (State, Move.From);
      To_Square := Get_Piece (State, Move.To);
      
      -- 1. Score captures using MVV-LVA (Most Valuable Victim - Least Valuable Attacker)
      if To_Square.Content = Occupied then
         -- Capture! Score by victim value - attacker value/10
         if From_Square.Content = Occupied then
            Score := 1000 + Piece_Value(To_Square.Piece) - Piece_Value(From_Square.Piece) / 10;
            -- Don't return early - also check for check+capture combinations
         end if;
      end if;
      
      -- 2. Check for killer moves (non-captures that caused cutoffs)
      if Depth <= Killer_Moves'Last(1) then
         if Move.From = Killer_Moves(Depth, 1).From and 
            Move.To = Killer_Moves(Depth, 1).To then
            return 900;  -- Primary killer
         elsif Move.From = Killer_Moves(Depth, 2).From and 
               Move.To = Killer_Moves(Depth, 2).To then
            return 800;  -- Secondary killer
         end if;
      end if;

      -- 3. History heuristic score for quiet moves (capped at 700)
      declare
         H : constant Natural :=
               History(Move.From.File, Move.From.Rank, Move.To.File, Move.To.Rank);
      begin
         if H > 0 then
            Score := Score + Integer'Min(H, 700);
         end if;
      end;
      
      -- 3. Check for checks (forcing moves)
      declare
         Test_State : Game_State := State;
      begin
         Test_State := Apply_Move (Test_State, Move);
         if Is_In_Check (Test_State, Test_State.Current_Player) then
            Score := Score + 10000;  -- Checks are critical - add huge bonus
         end if;
      end;
      
      return Score;
   end Score_Move;
   
   -- Sort moves by score (simple insertion sort, move lists are small)
   procedure Sort_Moves (
      State : in Game_State;
      Moves : in out Move_List;
      Depth : in Natural
   ) is
      type Scored_Move is record
         Move  : Move_Type;
         Score : Integer;
      end record;
      
      Len : constant Natural := Natural(Moves.Length);
   begin
      if Len <= 1 then
         return;  -- Nothing to sort
      end if;
      
      -- Score all moves
      declare
         type Scored_Array is array (1..Len) of Scored_Move;
         Scored : Scored_Array;
         Temp : Scored_Move;
         J : Integer;
      begin
         -- Create scored array
         for I in 1..Len loop
            Scored(I).Move := Moves.Element(I - 1);  -- Vector is 0-indexed
            Scored(I).Score := Score_Move(State, Scored(I).Move, Depth);
         end loop;
         
         -- Insertion sort (descending by score)
         for I in 2..Len loop
            Temp := Scored(I);
            J := I - 1;
            while J >= 1 and then Scored(J).Score < Temp.Score loop
               Scored(J + 1) := Scored(J);
               J := J - 1;
            end loop;
            Scored(J + 1) := Temp;
         end loop;
         
         -- Copy back to move list
         Moves.Clear;
         for I in 1..Len loop
            Moves.Append(Scored(I).Move);
         end loop;
      end;
   end Sort_Moves;
   
   -- Update killer moves when a move is good
   procedure Update_Killers (Move : Move_Type; Depth : Natural) is
   begin
      if Depth > Killer_Moves'Last(1) then
         return;
      end if;
      
      -- Don't store captures as killers (they're already scored high)
      -- We only care about quiet moves that are surprisingly good
      -- For simplicity in Phase 1, just update killers without capture check
      
      -- Update killers
      if not (Move.From = Killer_Moves(Depth, 1).From and 
              Move.To = Killer_Moves(Depth, 1).To) then
         -- Shift down and add new primary killer
         Killer_Moves(Depth, 2) := Killer_Moves(Depth, 1);
         Killer_Moves(Depth, 1) := Move;
      end if;
   end Update_Killers;

   -- ============================================================================
   -- PHASE 2: Alpha-Beta Pruning with Negamax
   -- ============================================================================
   
   -- Node counter for statistics
   Nodes_Searched : Natural := 0;
   Beta_Cutoffs : Natural := 0;
   
   -- PHASE 4: Additional performance counters
   Moves_Generated : Natural := 0;
   Moves_Sorted : Natural := 0;
   Apply_Move_Calls : Natural := 0;
   Checkmate_Tests : Natural := 0;
   
   procedure Reset_Statistics is
   begin
      Nodes_Searched := 0;
      Beta_Cutoffs := 0;
      Moves_Generated := 0;
      Moves_Sorted := 0;
      Apply_Move_Calls := 0;
      Checkmate_Tests := 0;
   end Reset_Statistics;
   
   -- Alpha-beta negamax search for FORCED MATE
   -- This is different from regular alpha-beta because:
   -- - At attacker depth: We need ANY move that forces mate (OR logic)
   -- - At defender depth: We need ALL moves to lead to mate (AND logic)
   -- Returns: True if forced mate exists, Best_Line contains the solution
   function Alpha_Beta_Mate_Search (
      Current_State : Game_State;
      Depth         : Natural;
      Target_Depth  : Natural;
      Is_Attacker   : Boolean;
      Our_Moves     : Move_List;
      Best_Line     : out Move_List
   ) return Boolean is
      Legal : Move_List := 
         Generate_All_Legal_Moves (Current_State, Current_State.Current_Player);
      Move_Line     : Move_List;
      Position_Hash : constant Zobrist_Hash := Compute_Hash(Current_State);
      Depth_Left    : constant Natural := Target_Depth - Depth;
      TT_Valid      : Boolean;
      TT_Mate       : Boolean;
   begin
      Nodes_Searched  := Nodes_Searched + 1;
      Moves_Generated := Moves_Generated + 1;

      -- Probe transposition table (only when enough depth remains to be useful)
      if Depth_Left >= TT_Min_Depth then
         declare
            Hit : constant Boolean :=
                     Probe_Table (Position_Hash, Depth_Left, TT_Valid, TT_Mate);
         begin
            if TT_Valid then
               Best_Line.Clear;
               return Hit;
            end if;
         end;
      end if;
      
      -- PHASE 1: Sort moves for better pruning
      Sort_Moves (Current_State, Legal, Depth);
      Moves_Sorted := Moves_Sorted + 1;  -- PHASE 4: Count sorts
      
      -- Check if we've reached target depth
      -- Target depth for mate-in-N: after attacker's Nth move, at depth 2N-1
      if not Is_Attacker and Depth = Target_Depth - 1 then
         -- After attacker's last move, check if defender is mated
         Checkmate_Tests := Checkmate_Tests + 1;  -- PHASE 4: Count checkmate tests
         if Legal.Is_Empty and then Is_Checkmate (Current_State, Current_State.Current_Player) then
            Best_Line := Our_Moves;
            return True;
         else
            Best_Line.Clear;
            return False;
         end if;
      end if;
      
      -- Don't search beyond target depth
      if Depth >= Target_Depth then
         Best_Line.Clear;
         return False;
      end if;
      
      -- If no legal moves before target depth, not a forced mate
      if Legal.Is_Empty then
         Best_Line.Clear;
         return False;
      end if;
      
      if Is_Attacker then
         -- Attacker: Need ANY move that forces mate (OR logic)
         for Move of Legal loop
            declare
               New_State : constant Game_State := Apply_Move (Current_State, Move);
               New_Moves : Move_List := Our_Moves;
            begin
               New_Moves.Append (Move);
               
               -- Try this move
               if Alpha_Beta_Mate_Search (
                  New_State, 
                  Depth + 1, 
                  Target_Depth,
                  False,  -- Now defender's turn
                  New_Moves,
                  Move_Line
               ) then
                  -- Found a mate! Record it and stop
                  Update_Killers (Move, Depth);
                  Update_History (Move, Depth_Left);
                  Best_Line := Move_Line;
                  -- PHASE 3: Store success in transposition table
                  if Depth_Left >= TT_Min_Depth then
                     Store_In_Table(Position_Hash, Depth_Left, True);
                  end if;
                  return True;
               end if;
            end;
         end loop;
         
         -- No move forces mate
         Best_Line.Clear;
         -- PHASE 3: Store failure in transposition table
         if Depth_Left >= TT_Min_Depth then
            Store_In_Table(Position_Hash, Depth_Left, False);
         end if;
         return False;
         
      else
         -- Defender: ALL moves must lead to mate (AND logic)
         -- If defender has ANY escape, attacker fails
         declare
            First_Line : Move_List;
            First_Found : Boolean := False;
         begin
            for Move of Legal loop
               declare
                  New_State : constant Game_State := Apply_Move (Current_State, Move);
               begin
                  -- Check if this defensive move escapes
                  if not Alpha_Beta_Mate_Search (
                     New_State,
                     Depth + 1,
                     Target_Depth,
                     True,  -- Back to attacker's turn
                     Our_Moves,  -- Don't include defender moves
                     Move_Line
                  ) then
                     -- Defender found an escape! Attacker fails
                     Beta_Cutoffs := Beta_Cutoffs + 1;
                     Best_Line.Clear;
                     -- PHASE 3: Store failure in transposition table
                     if Depth_Left >= TT_Min_Depth then
                        Store_In_Table(Position_Hash, Depth_Left, False);
                     end if;
                     return False;
                  end if;
                  
                  -- Save first successful line (all should lead to same result)
                  if not First_Found then
                     First_Line := Move_Line;
                     First_Found := True;
                  end if;
               end;
            end loop;
            
            -- All defender moves lead to mate!
            Best_Line := First_Line;
            -- PHASE 3: Store success in transposition table
            if Depth_Left >= TT_Min_Depth then
               Store_In_Table(Position_Hash, Depth_Left, True);
            end if;
            return True;
         end;
      end if;
   end Alpha_Beta_Mate_Search;

   -- ============================================================================
   -- Find_Mate_In_N with Alpha-Beta Pruning
   -- ============================================================================

   function Find_Mate_In_N (State : Game_State; 
                            N     : Positive;
                            Max_Depth : Positive := 10) return Solution_Type is
      pragma Unreferenced (Max_Depth);
      Result      : Solution_Type;
      Best_Line   : Move_List;
      Empty_Moves : Move_List;
      Found       : Boolean;
   begin
      Init_Killers;
      Reset_History;
      Reset_Statistics;
      Clear_Transposition_Table;

      Empty_Moves.Clear;

      -- Iterative deepening: search mate-in-1, mate-in-2, ..., mate-in-N.
      -- Each pass fills the TT and history table so the next pass starts
      -- with better move ordering. Exit as soon as a mate is found.
      for Iter in 1 .. N loop
         Found := Alpha_Beta_Mate_Search (
            Current_State => State,
            Depth         => 0,
            Target_Depth  => Iter * 2,
            Is_Attacker   => True,
            Our_Moves     => Empty_Moves,
            Best_Line     => Best_Line
         );

         -- Found a forced mate at this depth — report it
         if Found then
            Result.Found := True;
            Result.Moves := Best_Line;
            Result.Depth := Iter;
            return Result;
         end if;

         -- If we are below the requested depth we know there is no shallower
         -- mate, so continue with the next iteration.
      end loop;

      Result.Found := False;
      return Result;
   end Find_Mate_In_N;
   
   procedure Show_Solution (State : Game_State; Solution : Solution_Type) is
      Temp_State : Game_State := State;
      Move_Num : Positive := 1;
   begin
      if not Solution.Found then
         Put_Line ("No mate found.");
         return;
      end if;
      
      Put_Line ("Solution found! Mate in" & Natural'Image (Solution.Depth));
      Put_Line ("Moves:");
      
      -- Solution only contains attacker's moves, so display them sequentially
      for Move of Solution.Moves loop
         Put_Line (Positive'Image (Move_Num) & ". " & 
                   Move_To_Algebraic (Temp_State, Move));
         
         -- Apply move and show forced response if not the last move
         Temp_State := Apply_Move (Temp_State, Move);
         
         -- If not the last move, show defender's forced response
         if Move_Num < Positive (Solution.Moves.Length) then
            declare
               Defender_Moves : constant Move_List := 
                 Generate_All_Legal_Moves (Temp_State, Temp_State.Current_Player);
            begin
               if not Defender_Moves.Is_Empty then
                  -- Show one of the defender's responses (they all lose)
                  Put_Line ("   " & Move_To_Algebraic (Temp_State, 
                           Defender_Moves.First_Element) & " (forced)");
                  Temp_State := Apply_Move (Temp_State, Defender_Moves.First_Element);
               end if;
            end;
         end if;
         
         Move_Num := Move_Num + 1;
      end loop;
   end Show_Solution;
   
   -- PHASE 4: Print performance statistics
   procedure Print_Statistics is
   begin
      Put_Line("");
      Put_Line("=== Performance Statistics ===");
      Put_Line("Nodes searched:    " & Natural'Image(Nodes_Searched));
      Put_Line("Moves generated:   " & Natural'Image(Moves_Generated));
      Put_Line("Moves sorted:      " & Natural'Image(Moves_Sorted));
      Put_Line("Apply_Move calls:  " & Natural'Image(Apply_Move_Calls));
      Put_Line("Checkmate tests:   " & Natural'Image(Checkmate_Tests));
      Put_Line("Beta cutoffs:      " & Natural'Image(Beta_Cutoffs));
      Put_Line("");
      if Nodes_Searched > 0 then
         Put_Line("Avg moves/node:    " & Natural'Image(Moves_Generated * 100 / Nodes_Searched) & "%");
      end if;
   end Print_Statistics;

end Chess.Tactics;
