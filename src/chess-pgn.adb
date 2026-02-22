-- ***************************************************************************
--                      Chess - PGN Module Body
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

with Ada.Text_IO;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Characters.Handling; use Ada.Characters.Handling;
with Chess.Board; use Chess.Board;
with Chess.Notation; use Chess.Notation;

package body Chess.PGN is

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

   procedure Save_PGN (Filename : String;
                      Game     : PGN_Game) is
      package TIO renames Ada.Text_IO;
      File : TIO.File_Type;
      State : Game_State := Game.Initial_State;
      Move_Number : Positive := 1;
      Col : Natural := 0;
   begin
      TIO.Create (File, TIO.Out_File, Filename);
      
      -- Write metadata tags
      TIO.Put_Line (File, "[Event """ & Game.Event (1 .. Game.Event_Last) & """]");
      TIO.Put_Line (File, "[Site """ & Game.Site (1 .. Game.Site_Last) & """]");
      TIO.Put_Line (File, "[Date """ & Game.Date (1 .. Game.Date_Last) & """]");
      TIO.Put_Line (File, "[Round """ & Game.Round (1 .. Game.Round_Last) & """]");
      TIO.Put_Line (File, "[White """ & Game.White (1 .. Game.White_Last) & """]");
      TIO.Put_Line (File, "[Black """ & Game.Black (1 .. Game.Black_Last) & """]");
      TIO.Put_Line (File, "[Result """ & Game.Result (1 .. Game.Result_Last) & """]");
      TIO.New_Line (File);
      
      -- Write moves
      for Move of Game.Move_History loop
         declare
            Notation : constant String := Move_To_Algebraic (State, Move);
         begin
            if State.Current_Player = White then
               TIO.Put (File, Positive'Image (Move_Number) & ". " & Notation & " ");
               Col := Col + Notation'Length + 4;
            else
               TIO.Put (File, Notation & " ");
               Col := Col + Notation'Length + 1;
               Move_Number := Move_Number + 1;
            end if;
            
            State := Apply_Move (State, Move);
            
            -- Wrap at 80 columns
            if Col > 70 then
               TIO.New_Line (File);
               Col := 0;
            end if;
         end;
      end loop;
      
      TIO.Put_Line (File, Game.Result (1 .. Game.Result_Last));
      
      TIO.Close (File);
   end Save_PGN;

   function Load_PGN (Filename : String) return PGN_Game is
      package TIO renames Ada.Text_IO;
      File : TIO.File_Type;
      Game : PGN_Game;
      Line : String (1 .. 500);
      Last : Natural;
      State : Game_State;
      Has_FEN : Boolean := False;
      FEN_String : String (1 .. 200) := (others => ' ');
      FEN_Last : Natural := 0;
   begin
      TIO.Open (File, TIO.In_File, Filename);
      
      Game.Initial_State := Initial_Position;
      State := Game.Initial_State;
      
      -- Read file line by line
      while not TIO.End_Of_File (File) loop
         TIO.Get_Line (File, Line, Last);
         
         if Last > 0 then
            -- Parse metadata tags
            if Line (1) = '[' then
               declare
                  Tag_End : constant Natural := Index (Line (1 .. Last), " ");
                  Value_Start : Natural;
                  Value_End : Natural;
               begin
                  if Tag_End > 0 then
                     Value_Start := Index (Line (1 .. Last), """");
                     if Value_Start > 0 then
                        Value_End := Index (Line (Value_Start + 1 .. Last), """");
                        if Value_End > 0 then
                           Value_End := Value_Start + Value_End - 1;
                           declare
                              Tag : constant String := Line (2 .. Tag_End - 1);
                              Value : constant String := Line (Value_Start + 1 .. Value_End - 1);
                           begin
                              if Tag = "Event" then
                                 Game.Event (1 .. Value'Length) := Value;
                                 Game.Event_Last := Value'Length;
                              elsif Tag = "Site" then
                                 Game.Site (1 .. Value'Length) := Value;
                                 Game.Site_Last := Value'Length;
                              elsif Tag = "Date" then
                                 Game.Date (1 .. Value'Length) := Value;
                                 Game.Date_Last := Value'Length;
                              elsif Tag = "Round" then
                                 Game.Round (1 .. Value'Length) := Value;
                                 Game.Round_Last := Value'Length;
                              elsif Tag = "White" then
                                 Game.White (1 .. Value'Length) := Value;
                                 Game.White_Last := Value'Length;
                              elsif Tag = "Black" then
                                 Game.Black (1 .. Value'Length) := Value;
                                 Game.Black_Last := Value'Length;
                              elsif Tag = "Result" then
                                 Game.Result (1 .. Value'Length) := Value;
                                 Game.Result_Last := Value'Length;
                              elsif Tag = "FEN" then
                                 FEN_String (1 .. Value'Length) := Value;
                                 FEN_Last := Value'Length;
                                 Has_FEN := True;
                              end if;
                           end;
                        end if;
                     end if;
                  end if;
               end;
            elsif Line (1) /= '[' and Trim (Line (1 .. Last), Ada.Strings.Both)'Length > 0 then
               -- Parse moves
               declare
                  Moves_Line : constant String := Trim (Line (1 .. Last), Ada.Strings.Both);
                  Idx : Positive := Moves_Line'First;
               begin
                  while Idx <= Moves_Line'Last loop
                     -- Skip move numbers and periods
                     while Idx <= Moves_Line'Last and then
                           (Moves_Line (Idx) in '0' .. '9' or Moves_Line (Idx) = '.') loop
                        Idx := Idx + 1;
                     end loop;
                     
                     -- Skip whitespace
                     while Idx <= Moves_Line'Last and then Moves_Line (Idx) = ' ' loop
                        Idx := Idx + 1;
                     end loop;
                     
                     if Idx <= Moves_Line'Last then
                        -- Find end of move notation
                        declare
                           Move_Start : constant Positive := Idx;
                           Move_End : Natural := Idx;
                        begin
                           while Move_End <= Moves_Line'Last and then
                                 Moves_Line (Move_End) /= ' ' loop
                              Move_End := Move_End + 1;
                           end loop;
                           Move_End := Move_End - 1;
                           
                           declare
                              Move_Str : constant String := Moves_Line (Move_Start .. Move_End);
                           begin
                              -- Check for result markers
                              if Move_Str = "1-0" or Move_Str = "0-1" or 
                                 Move_Str = "1/2-1/2" or Move_Str = "*" then
                                 exit;
                              end if;
                              
                              -- Parse and apply move
                              begin
                                 declare
                                    Move : constant Move_Type := Parse_Move (State, Move_Str);
                                 begin
                                    Game.Move_History.Append (Move);
                                    State := Apply_Move (State, Move);
                                 end;
                              exception
                                 when others =>
                                    -- Skip unparseable moves
                                    null;
                              end;
                              
                              Idx := Move_End + 1;
                           end;
                        end;
                     end if;
                  end loop;
               end;
            end if;
         end if;
      end loop;
      
      TIO.Close (File);
      
      -- Apply FEN if present
      if Has_FEN then
         Game.Initial_State := FEN_To_Game_State (FEN_String (1 .. FEN_Last));
      end if;
      
      return Game;
   end Load_PGN;

   function FEN_To_Game_State (FEN : String) return Game_State is
      State : Game_State;
      Idx : Positive := FEN'First;
      Current_File : Chess.Types.File_Type := File_A;
      Current_Rank : Rank_Type := 8;
   begin
      Clear_Board (State);
      
      -- Parse piece placement
      while Idx <= FEN'Last and then FEN (Idx) /= ' ' loop
         if FEN (Idx) = '/' then
            Current_Rank := Current_Rank - 1;
            Current_File := File_A;
         elsif FEN (Idx) in '1' .. '8' then
            -- Skip empty squares
            declare
               Skip : constant Natural := Character'Pos (FEN (Idx)) - Character'Pos ('0');
            begin
               for I in 1 .. Skip loop
                  if Current_File < File_H then
                     Current_File := Chess.Types.File_Type'Succ (Current_File);
                  else
                     exit;
                  end if;
               end loop;
            end;
         else
            -- Place piece
            declare
               Color : constant Color_Type := 
                 (if Is_Upper (FEN (Idx)) then White else Black);
               Kind : Piece_Kind;
            begin
               case To_Upper (FEN (Idx)) is
                  when 'P' => Kind := Pawn;
                  when 'N' => Kind := Knight;
                  when 'B' => Kind := Bishop;
                  when 'R' => Kind := Rook;
                  when 'Q' => Kind := Queen;
                  when 'K' => Kind := King;
                  when others => raise PGN_Error with "Invalid piece in FEN";
               end case;
               
               Place_Piece (State, (Current_File, Current_Rank), (Kind, Color));
               if Current_File < File_H then
                  Current_File := Chess.Types.File_Type'Succ (Current_File);
               end if;
            end;
         end if;
         Idx := Idx + 1;
      end loop;
      
      -- Parse active color
      Idx := Idx + 1;  -- Skip space
      if Idx <= FEN'Last then
         State.Current_Player := (if FEN (Idx) = 'w' then White else Black);
         Idx := Idx + 1;
      end if;
      
      -- Parse castling rights
      Idx := Idx + 1;  -- Skip space
      State.Castling := (others => False);
      if Idx <= FEN'Last then
         while Idx <= FEN'Last and then FEN (Idx) /= ' ' loop
            case FEN (Idx) is
               when 'K' => State.Castling.White_Kingside := True;
               when 'Q' => State.Castling.White_Queenside := True;
               when 'k' => State.Castling.Black_Kingside := True;
               when 'q' => State.Castling.Black_Queenside := True;
               when '-' => null;
               when others => null;
            end case;
            Idx := Idx + 1;
         end loop;
      end if;
      
      -- Parse en passant
      Idx := Idx + 1;  -- Skip space
      State.Has_En_Passant := False;
      if Idx <= FEN'Last and then FEN (Idx) /= '-' then
         if Idx + 1 <= FEN'Last then
            State.En_Passant := Parse_Square (FEN (Idx .. Idx + 1));
            State.Has_En_Passant := True;
         end if;
      end if;
      
      return State;
   end FEN_To_Game_State;

   function Game_State_To_FEN (State : Game_State) return String is
      Result : String (1 .. 100) := (others => ' ');
      Idx : Positive := 1;
      Empty_Count : Natural;
   begin
      -- Piece placement
      for R in reverse Rank_Type loop
         Empty_Count := 0;
         for F in Chess.Types.File_Type loop
            declare
               Square : constant Board_Square := Get_Piece (State, (F, R));
            begin
               if Square.Content = Empty then
                  Empty_Count := Empty_Count + 1;
               else
                  if Empty_Count > 0 then
                     Result (Idx) := Character'Val (Character'Pos ('0') + Empty_Count);
                     Idx := Idx + 1;
                     Empty_Count := 0;
                  end if;
                  
                  declare
                     C : Character := Piece_To_ASCII (Square.Piece);
                  begin
                     Result (Idx) := C;
                     Idx := Idx + 1;
                  end;
               end if;
            end;
         end loop;
         
         if Empty_Count > 0 then
            Result (Idx) := Character'Val (Character'Pos ('0') + Empty_Count);
            Idx := Idx + 1;
         end if;
         
         if R > Rank_Type'First then
            Result (Idx) := '/';
            Idx := Idx + 1;
         end if;
      end loop;
      
      Result (Idx) := ' ';
      Idx := Idx + 1;
      
      -- Active color
      Result (Idx) := (if State.Current_Player = White then 'w' else 'b');
      Idx := Idx + 1;
      Result (Idx) := ' ';
      Idx := Idx + 1;
      
      -- Castling rights
      if State.Castling.White_Kingside or State.Castling.White_Queenside or
         State.Castling.Black_Kingside or State.Castling.Black_Queenside then
         if State.Castling.White_Kingside then
            Result (Idx) := 'K';
            Idx := Idx + 1;
         end if;
         if State.Castling.White_Queenside then
            Result (Idx) := 'Q';
            Idx := Idx + 1;
         end if;
         if State.Castling.Black_Kingside then
            Result (Idx) := 'k';
            Idx := Idx + 1;
         end if;
         if State.Castling.Black_Queenside then
            Result (Idx) := 'q';
            Idx := Idx + 1;
         end if;
      else
         Result (Idx) := '-';
         Idx := Idx + 1;
      end if;
      
      Result (Idx) := ' ';
      Idx := Idx + 1;
      
      -- En passant
      if State.Has_En_Passant then
         declare
            Square_Str : constant String := Square_To_String (State.En_Passant);
         begin
            Result (Idx .. Idx + 1) := Square_Str;
            Idx := Idx + 2;
         end;
      else
         Result (Idx) := '-';
         Idx := Idx + 1;
      end if;
      
      Result (Idx) := ' ';
      Idx := Idx + 1;
      
      -- Halfmove clock
      declare
         Clock_Str : constant String := Trim (Natural'Image (State.Halfmove_Clock), Ada.Strings.Both);
      begin
         Result (Idx .. Idx + Clock_Str'Length - 1) := Clock_Str;
         Idx := Idx + Clock_Str'Length;
      end;
      
      Result (Idx) := ' ';
      Idx := Idx + 1;
      
      -- Fullmove number
      declare
         Move_Str : constant String := Trim (Positive'Image (State.Fullmove_Number), Ada.Strings.Both);
      begin
         Result (Idx .. Idx + Move_Str'Length - 1) := Move_Str;
         Idx := Idx + Move_Str'Length;
      end;
      
      return Trim (Result (1 .. Idx - 1), Ada.Strings.Both);
   end Game_State_To_FEN;

end Chess.PGN;
