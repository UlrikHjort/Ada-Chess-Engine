-- ***************************************************************************
--                      Chess - Tactical Analysis Specification
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

with Chess.Types; use Chess.Types;
with Chess.Moves; use Chess.Moves;

package Chess.Tactics is

   type Solution_Type is record
      Found : Boolean := False;
      Moves : Move_List;
      Depth : Natural := 0;
   end record;

   function Find_Mate_In_N (State : Game_State; 
                           N     : Positive;
                           Max_Depth : Positive := 10) return Solution_Type;

   procedure Show_Solution (State : Game_State; Solution : Solution_Type);
   
   -- PHASE 4: Print performance statistics
   procedure Print_Statistics;

end Chess.Tactics;
