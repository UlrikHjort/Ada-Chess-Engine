-- ***************************************************************************
--                      Chess - UCI Main Program
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
-- main_uci.adb: Entry point for the UCI-mode binary.
--
-- Build:  gnatmake ... src/main_uci.adb -o bin/chess_uci
-- Usage:  bin/chess_uci   (then talk UCI on stdin/stdout)
-- ***************************************************************************

with Chess.Hash;
with Chess.Bitboard;
with Chess.UCI; use Chess.UCI;

procedure Main_UCI is
begin
   Chess.Hash.Initialize_Zobrist;
   Chess.Bitboard.Initialize_Bitboards;
   Run_UCI_Loop;
end Main_UCI;
