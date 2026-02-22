-- ***************************************************************************
--                      Chess - Zobrist Hashing Implementation
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

with Chess.Board; use Chess.Board;

package body Chess.Hash is

   -- Zobrist random numbers for each piece/square/color combination
   type Piece_Square_Table is array (Piece_Kind, Color_Type, File_Type, Rank_Type) of Zobrist_Hash;
   Zobrist_Pieces : Piece_Square_Table;
   Zobrist_Black_To_Move : Zobrist_Hash;
   
   -- Simple pseudo-random number generator (Linear Congruential Generator)
   Seed : Unsigned_64 := 16#123456789ABCDEF0#;
   
   function Random return Zobrist_Hash is
   begin
      -- LCG parameters from Knuth
      Seed := Seed * 6364136223846793005 + 1442695040888963407;
      return Zobrist_Hash(Seed);
   end Random;
   
   procedure Initialize_Zobrist is
   begin
      -- Generate random numbers for each piece/color/square combination
      for Piece in Piece_Kind loop
         for Color in Color_Type loop
            for File in File_Type loop
               for Rank in Rank_Type loop
                  Zobrist_Pieces(Piece, Color, File, Rank) := Random;
               end loop;
            end loop;
         end loop;
      end loop;
      
      Zobrist_Black_To_Move := Random;
   end Initialize_Zobrist;
   
   function Compute_Hash (State : Game_State) return Zobrist_Hash is
      Hash : Zobrist_Hash := 0;
   begin
      -- XOR in all pieces on the board
      for File in File_Type loop
         for Rank in Rank_Type loop
            declare
               Square : constant Square_Type := (File, Rank);
               BS : constant Board_Square := Get_Piece(State, Square);
            begin
               if BS.Content = Occupied then
                  Hash := Hash xor Zobrist_Pieces(BS.Piece.Kind, BS.Piece.Color, File, Rank);
               end if;
            end;
         end loop;
      end loop;
      
      -- XOR in side to move
      if State.Current_Player = Black then
         Hash := Hash xor Zobrist_Black_To_Move;
      end if;
      
      return Hash;
   end Compute_Hash;
   
   function Update_Hash (
      Old_Hash : Zobrist_Hash;
      State    : Game_State;
      Move     : Move_Type
   ) return Zobrist_Hash is
      Hash : Zobrist_Hash := Old_Hash;
      From_Square : constant Board_Square := Get_Piece(State, Move.From);
      To_Square : constant Board_Square := Get_Piece(State, Move.To);
   begin
      -- XOR out piece from source square
      if From_Square.Content = Occupied then
         Hash := Hash xor Zobrist_Pieces(From_Square.Piece.Kind, From_Square.Piece.Color, Move.From.File, Move.From.Rank);
      end if;
      
      -- XOR out captured piece (if any)
      if To_Square.Content = Occupied then
         Hash := Hash xor Zobrist_Pieces(To_Square.Piece.Kind, To_Square.Piece.Color, Move.To.File, Move.To.Rank);
      end if;
      
      -- XOR in piece at destination square
      if From_Square.Content = Occupied then
         -- Handle pawn promotion
         if Move.Flag = Promotion then
            Hash := Hash xor Zobrist_Pieces(Move.Promotion, From_Square.Piece.Color, Move.To.File, Move.To.Rank);
         else
            Hash := Hash xor Zobrist_Pieces(From_Square.Piece.Kind, From_Square.Piece.Color, Move.To.File, Move.To.Rank);
         end if;
      end if;
      
      -- Toggle side to move
      Hash := Hash xor Zobrist_Black_To_Move;
      
      return Hash;
   end Update_Hash;

end Chess.Hash;
