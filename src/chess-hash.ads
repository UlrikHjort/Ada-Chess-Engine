-- ***************************************************************************
--                      Chess - Zobrist Hashing Specification
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
with Interfaces; use Interfaces;

package Chess.Hash is

   type Zobrist_Hash is new Unsigned_64;
   
   -- Initialize Zobrist random numbers (call once at program start)
   procedure Initialize_Zobrist;
   
   -- Compute hash for a complete position
   function Compute_Hash (State : Game_State) return Zobrist_Hash;
   
   -- Update hash incrementally after a move
   function Update_Hash (
      Old_Hash : Zobrist_Hash;
      State    : Game_State;
      Move     : Move_Type
   ) return Zobrist_Hash;

end Chess.Hash;
