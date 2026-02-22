-- ***************************************************************************
--                      Chess - PGN Module Specification
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

package Chess.PGN is

   PGN_Error : exception;

   type PGN_Game is record
      Event      : String (1 .. 100) := (others => ' ');
      Event_Last : Natural := 0;
      Site       : String (1 .. 100) := (others => ' ');
      Site_Last  : Natural := 0;
      Date       : String (1 .. 20) := (others => ' ');
      Date_Last  : Natural := 0;
      Round      : String (1 .. 20) := (others => ' ');
      Round_Last : Natural := 0;
      White      : String (1 .. 100) := (others => ' ');
      White_Last : Natural := 0;
      Black      : String (1 .. 100) := (others => ' ');
      Black_Last : Natural := 0;
      Result     : String (1 .. 10) := (others => ' ');
      Result_Last: Natural := 0;
      Initial_State : Game_State;
      Move_History : Move_List;
   end record;

   procedure Save_PGN (Filename : String;
                      Game     : PGN_Game);

   function Load_PGN (Filename : String) return PGN_Game;

   function FEN_To_Game_State (FEN : String) return Game_State;

   function Game_State_To_FEN (State : Game_State) return String;

end Chess.PGN;
