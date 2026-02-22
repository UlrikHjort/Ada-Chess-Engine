-- ***************************************************************************
--                      Chess - Display Module Body
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

package body Chess.Display is

   function Piece_To_Unicode (Piece : Piece_Type) return String is
   begin
      case Piece.Color is
         when White =>
            case Piece.Kind is
               when King   => return "♔";
               when Queen  => return "♕";
               when Rook   => return "♖";
               when Bishop => return "♗";
               when Knight => return "♘";
               when Pawn   => return "♙";
            end case;
         when Black =>
            case Piece.Kind is
               when King   => return "♚";
               when Queen  => return "♛";
               when Rook   => return "♜";
               when Bishop => return "♝";
               when Knight => return "♞";
               when Pawn   => return "♟";
            end case;
      end case;
   end Piece_To_Unicode;

   function Piece_To_ASCII (Piece : Piece_Type) return Character is
   begin
      case Piece.Color is
         when White =>
            case Piece.Kind is
               when King   => return 'K';
               when Queen  => return 'Q';
               when Rook   => return 'R';
               when Bishop => return 'B';
               when Knight => return 'N';
               when Pawn   => return 'P';
            end case;
         when Black =>
            case Piece.Kind is
               when King   => return 'k';
               when Queen  => return 'q';
               when Rook   => return 'r';
               when Bishop => return 'b';
               when Knight => return 'n';
               when Pawn   => return 'p';
            end case;
      end case;
   end Piece_To_ASCII;

   procedure Show_Board (State : Game_State; Use_Unicode : Boolean := True) is
   begin
      New_Line;
      Put_Line ("  ┌───┬───┬───┬───┬───┬───┬───┬───┐");
      
      for R in reverse Rank_Type loop
         Put (Rank_Type'Image (R)(2) & " │");
         
         for F in Chess.Types.File_Type loop
            declare
               Square : constant Board_Square := State.Board (F, R);
            begin
               if Square.Content = Occupied then
                  if Use_Unicode then
                     Put (" " & Piece_To_Unicode (Square.Piece) & " │");
                  else
                     Put (" " & Piece_To_ASCII (Square.Piece) & " │");
                  end if;
               else
                  Put ("   │");
               end if;
            end;
         end loop;
         
         New_Line;
         
         if R > Rank_Type'First then
            Put_Line ("  ├───┼───┼───┼───┼───┼───┼───┼───┤");
         end if;
      end loop;
      
      Put_Line ("  └───┴───┴───┴───┴───┴───┴───┴───┘");
      Put_Line ("    a   b   c   d   e   f   g   h");
      New_Line;
   end Show_Board;

   procedure Show_Game_Info (State : Game_State) is
   begin
      Put_Line ("┌─────────────────────────────────────┐");
      Put_Line ("│          Game Information           │");
      Put_Line ("├─────────────────────────────────────┤");
      Put ("│ Current Player: ");
      if State.Current_Player = White then
         Put_Line ("White               │");
      else
         Put_Line ("Black               │");
      end if;
      Put_Line ("│ Move: " & Positive'Image (State.Fullmove_Number) & 
                "                           │");
      Put_Line ("│ Halfmove Clock: " & Natural'Image (State.Halfmove_Clock) &
                "                    │");
      Put_Line ("└─────────────────────────────────────┘");
      New_Line;
   end Show_Game_Info;

end Chess.Display;
