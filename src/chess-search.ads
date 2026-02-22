-- ***************************************************************************
--                      Chess - Negamax Search Engine Specification
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
--
-- Chess.Search: General-purpose negamax alpha-beta search.
--
-- Unlike Chess.Tactics (which searches for forced mates only),
-- this package finds the BEST MOVE in any position by exploring the
-- game tree to a given depth and evaluating leaf nodes with Chess.Eval.
--
-- Usage:
--   Move := Find_Best_Move (State, Depth => 4);
-- ***************************************************************************

with Chess.Types; use Chess.Types;
with Chess.Hash;  use Chess.Hash;

package Chess.Search is

   -- Default search depth (plies). 4 = solid play, 5 = stronger but slower.
   Default_Depth : constant Positive := 4;

   -- Maximum depth for iterative deepening (practical ceiling)
   Max_ID_Depth  : constant Positive := 20;

   -- History of position hashes for repetition detection (max 500 half-moves)
   Max_Game_History : constant := 500;
   type Hash_Array is array (0 .. Max_Game_History - 1) of Zobrist_Hash;

   -- Raises No_Legal_Moves when the position is checkmate or stalemate.
   No_Legal_Moves : exception;

   -- Find the best move searching to a fixed depth.
   -- History / History_Count: Zobrist hashes of prior positions (for draw
   -- detection).  Leave at default if not needed.
   function Find_Best_Move (State         : Game_State;
                            Depth         : Positive  := Default_Depth;
                            Verbose       : Boolean   := False;
                            History       : Hash_Array := (others => 0);
                            History_Count : Natural   := 0)
                            return Move_Type;

   -- Find the best move using iterative deepening within a time budget.
   -- Time_Ms: milliseconds available for the search.
   -- Returns the best move from the last fully-completed depth.
   function Find_Best_Move_Timed (State         : Game_State;
                                  Time_Ms       : Positive;
                                  Verbose       : Boolean   := False;
                                  History       : Hash_Array := (others => 0);
                                  History_Count : Natural   := 0)
                                  return Move_Type;

   -- Print statistics from the last search call.
   procedure Print_Search_Stats;

end Chess.Search;
