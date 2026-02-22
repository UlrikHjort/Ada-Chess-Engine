-- ***************************************************************************
--                      Chess - Board Module Body
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

package body Chess.Board is

   procedure Clear_Board (State : in out Game_State) is
   begin
      for F in File_Type loop
         for R in Rank_Type loop
            State.Board (F, R) := (Content => Empty);
         end loop;
      end loop;
   end Clear_Board;

   procedure Place_Piece (State  : in out Game_State;
                         Square : Square_Type;
                         Piece  : Piece_Type) is
   begin
      State.Board (Square.File, Square.Rank) := 
        (Content => Occupied, Piece => Piece);
   end Place_Piece;

   function Get_Piece (State  : Game_State;
                      Square : Square_Type) return Board_Square is
   begin
      return State.Board (Square.File, Square.Rank);
   end Get_Piece;

   function Initial_Position return Game_State is
      State : Game_State;
   begin
      Clear_Board (State);
      
      -- White pieces
      Place_Piece (State, (File_A, 1), (Rook, White));
      Place_Piece (State, (File_B, 1), (Knight, White));
      Place_Piece (State, (File_C, 1), (Bishop, White));
      Place_Piece (State, (File_D, 1), (Queen, White));
      Place_Piece (State, (File_E, 1), (King, White));
      Place_Piece (State, (File_F, 1), (Bishop, White));
      Place_Piece (State, (File_G, 1), (Knight, White));
      Place_Piece (State, (File_H, 1), (Rook, White));
      
      for F in File_Type loop
         Place_Piece (State, (F, 2), (Pawn, White));
      end loop;
      
      -- Black pieces
      Place_Piece (State, (File_A, 8), (Rook, Black));
      Place_Piece (State, (File_B, 8), (Knight, Black));
      Place_Piece (State, (File_C, 8), (Bishop, Black));
      Place_Piece (State, (File_D, 8), (Queen, Black));
      Place_Piece (State, (File_E, 8), (King, Black));
      Place_Piece (State, (File_F, 8), (Bishop, Black));
      Place_Piece (State, (File_G, 8), (Knight, Black));
      Place_Piece (State, (File_H, 8), (Rook, Black));
      
      for F in File_Type loop
         Place_Piece (State, (F, 7), (Pawn, Black));
      end loop;
      
      State.Current_Player := White;
      State.Castling := (others => True);
      State.Has_En_Passant := False;
      State.Halfmove_Clock := 0;
      State.Fullmove_Number := 1;
      
      return State;
   end Initial_Position;

end Chess.Board;
