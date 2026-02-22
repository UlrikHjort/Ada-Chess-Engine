-- ***************************************************************************
--                      Chess - Core Types Body
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

package body Chess.Types is

   function Opposite_Color (Color : Color_Type) return Color_Type is
   begin
      if Color = White then
         return Black;
      else
         return White;
      end if;
   end Opposite_Color;

   function File_To_Char (File : File_Type) return Character is
   begin
      return Character'Val (Character'Pos ('a') + File_Type'Pos (File));
   end File_To_Char;

   function Char_To_File (C : Character) return File_Type is
   begin
      return File_Type'Val (Character'Pos (C) - Character'Pos ('a'));
   end Char_To_File;

   function Square_To_String (Square : Square_Type) return String is
   begin
      return File_To_Char (Square.File) & Rank_Type'Image (Square.Rank)(2);
   end Square_To_String;

end Chess.Types;
