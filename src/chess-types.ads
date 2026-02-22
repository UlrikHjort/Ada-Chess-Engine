-- ***************************************************************************
--                      Chess - Core Types Specification
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

package Chess.Types is

   type Color_Type is (White, Black);

   type Piece_Kind is (Pawn, Knight, Bishop, Rook, Queen, King);

   type Piece_Type is record
      Kind  : Piece_Kind;
      Color : Color_Type;
   end record;

   type File_Type is (File_A, File_B, File_C, File_D, File_E, File_F, File_G, File_H);
   type Rank_Type is range 1 .. 8;

   type Square_Type is record
      File : File_Type;
      Rank : Rank_Type;
   end record;

   type Square_Content is (Empty, Occupied);

   type Board_Square (Content : Square_Content := Empty) is record
      case Content is
         when Empty =>
            null;
         when Occupied =>
            Piece : Piece_Type;
      end case;
   end record;

   type Board_Type is array (File_Type, Rank_Type) of Board_Square;

   type Castling_Rights is record
      White_Kingside  : Boolean := True;
      White_Queenside : Boolean := True;
      Black_Kingside  : Boolean := True;
      Black_Queenside : Boolean := True;
   end record;

   type Move_Flag is (Normal, Castle_Kingside, Castle_Queenside, 
                      En_Passant, Promotion);

   type Move_Type is record
      From      : Square_Type;
      To        : Square_Type;
      Flag      : Move_Flag := Normal;
      Promotion : Piece_Kind := Queen;  -- Default promotion to Queen
   end record;

   type Game_State is record
      Board          : Board_Type;
      Current_Player : Color_Type := White;
      Castling       : Castling_Rights;
      En_Passant     : Square_Type;
      Has_En_Passant : Boolean := False;
      Halfmove_Clock : Natural := 0;  -- For 50-move rule
      Fullmove_Number: Positive := 1;
   end record;

   function Opposite_Color (Color : Color_Type) return Color_Type;

   function File_To_Char (File : File_Type) return Character;
   function Char_To_File (C : Character) return File_Type;
   
   function Square_To_String (Square : Square_Type) return String;

end Chess.Types;
