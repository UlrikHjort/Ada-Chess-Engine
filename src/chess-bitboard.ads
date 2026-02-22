-- ***************************************************************************
--                      Chess - Bitboard Foundation Specification
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
-- Bitboard Foundation Module
--
-- A bitboard represents the entire 8x8 board as a single 64-bit integer.
-- Each bit corresponds to one square.  Bit index = (rank-1)*8 + file_index
-- where file_index: a=0, b=1, ..., h=7 and rank: 1..8.
--
-- Example: square e4 → file_index=4, rank=4 → bit (3*8+4) = bit 28
--
-- This module is a STANDALONE FOUNDATION.  It does not replace the
-- existing array-based board; it runs alongside as a demonstration and
-- future building block for a high-performance move generator.
-- ***************************************************************************

with Chess.Types;      use Chess.Types;
with Interfaces;

package Chess.Bitboard is

   -- The fundamental type: a 64-bit set of squares
   subtype Bitboard is Interfaces.Unsigned_64;

   -- Useful constants
   Empty_Board : constant Bitboard := 0;
   Full_Board  : constant Bitboard := Interfaces.Unsigned_64'Last;

   -- File masks (all squares on a given file)
   File_A_Mask : constant Bitboard := 16#0101010101010101#;
   File_B_Mask : constant Bitboard := 16#0202020202020202#;
   File_C_Mask : constant Bitboard := 16#0404040404040404#;
   File_D_Mask : constant Bitboard := 16#0808080808080808#;
   File_E_Mask : constant Bitboard := 16#1010101010101010#;
   File_F_Mask : constant Bitboard := 16#2020202020202020#;
   File_G_Mask : constant Bitboard := 16#4040404040404040#;
   File_H_Mask : constant Bitboard := 16#8080808080808080#;

   -- Rank masks (all squares on a given rank)
   Rank_1_Mask : constant Bitboard := 16#00000000000000FF#;
   Rank_2_Mask : constant Bitboard := 16#000000000000FF00#;
   Rank_3_Mask : constant Bitboard := 16#0000000000FF0000#;
   Rank_4_Mask : constant Bitboard := 16#00000000FF000000#;
   Rank_5_Mask : constant Bitboard := 16#000000FF00000000#;
   Rank_6_Mask : constant Bitboard := 16#0000FF0000000000#;
   Rank_7_Mask : constant Bitboard := 16#00FF000000000000#;
   Rank_8_Mask : constant Bitboard := 16#FF00000000000000#;

   -- Convert a chess square to its bit index (0..63)
   function Square_To_Bit (Sq : Square_Type) return Natural;

   -- Set/clear/test a single square in a bitboard
   function Set_Square   (BB : Bitboard; Sq : Square_Type) return Bitboard;
   function Clear_Square (BB : Bitboard; Sq : Square_Type) return Bitboard;
   function Test_Square  (BB : Bitboard; Sq : Square_Type) return Boolean;

   -- Count the number of set bits (popcount)
   function Popcount (BB : Bitboard) return Natural;

   -- Find the lowest set bit index (0..63); undefined if BB = 0
   function LSB (BB : Bitboard) return Natural;

   -- Remove the lowest set bit and return it as a bitboard
   function Pop_LSB (BB : in out Bitboard) return Natural;

   -- -------------------------------------------------------------------------
   -- Pre-computed attack tables
   -- -------------------------------------------------------------------------

   -- Knight attacks from every square
   Knight_Attacks : array (0 .. 63) of Bitboard;

   -- King attacks from every square
   King_Attacks : array (0 .. 63) of Bitboard;

   -- Pawn attacks by color and square
   Pawn_Attacks : array (Color_Type, 0 .. 63) of Bitboard;

   -- Initialize all pre-computed attack tables (call once at startup)
   procedure Initialize_Bitboards;

   -- -------------------------------------------------------------------------
   -- Sliding piece attack generation (classical ray-casting)
   -- -------------------------------------------------------------------------

   -- Rook attacks from a given square with an occupancy mask
   function Rook_Attacks   (Sq : Natural; Occupied : Bitboard) return Bitboard;

   -- Bishop attacks from a given square with an occupancy mask
   function Bishop_Attacks (Sq : Natural; Occupied : Bitboard) return Bitboard;

   -- Queen = Rook + Bishop
   function Queen_Attacks  (Sq : Natural; Occupied : Bitboard) return Bitboard;

   -- -------------------------------------------------------------------------
   -- Conversion utilities
   -- -------------------------------------------------------------------------

   -- Build a combined occupancy bitboard from a game state
   function Occupancy      (State : Game_State) return Bitboard;
   function White_Pieces   (State : Game_State) return Bitboard;
   function Black_Pieces   (State : Game_State) return Bitboard;
   function Pieces_Of_Kind (State : Game_State;
                             Color : Color_Type;
                             Kind  : Piece_Kind) return Bitboard;

   -- Returns True when Square is attacked by any piece of By_Color.
   -- Uses precomputed attack tables — much faster than generating pseudo-legal moves.
   -- Requires Initialize_Bitboards to have been called once at start-up.
   function Is_Square_Attacked_BB (State    : Game_State;
                                   Square   : Square_Type;
                                   By_Color : Color_Type) return Boolean;

   -- Print a bitboard in 8x8 grid form (for debugging)
   procedure Print_Bitboard (BB : Bitboard; Label : String := "");

end Chess.Bitboard;
