-- ***************************************************************************
--                      Chess - UCI Protocol Specification
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
-- Chess.UCI: Universal Chess Interface protocol loop.
--
-- Implements the subset of UCI needed to connect to GUIs such as Arena,
-- Cutechess, and lichess-bot.  Supported commands:
--
--   uci          → id + option lines + uciok
--   isready      → readyok
--   ucinewgame   → resets internal state
--   position     → sets up position from startpos or FEN + optional moves
--   go           → searches and replies with bestmove
--   stop         → (graceful; search is single-threaded so not needed)
--   quit         → exits
--
-- All output goes to stdout (as required by UCI).
-- ***************************************************************************

package Chess.UCI is

   -- Enter the UCI command loop. Does not return until "quit" is received.
   procedure Run_UCI_Loop;

end Chess.UCI;
