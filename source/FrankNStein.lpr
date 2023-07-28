{
  Frank N Stein Resurrected - Copyright 2023 MKSZTSZ
  Written by Szabó "Gilby" Zsolt / MKSZTSZ

  This file is part of the source code of Frank N Stein Resurrected.

  Frank N Stein Resurrected is free software: you can redistribute it
  and/or modify it under the terms of the GNU General Public License
  as published by the Free Software Foundation, either version 3 of the License,
  or (at your option) any later version.

  Frank N Stein Resurrected is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
  See the GNU General Public License for more details.

  You should have received a copy of the GNU General Public License along with
  Frank N Stein Resurrected. If not, see <https://www.gnu.org/licenses/>.
}

program FrankNStein;

{$ifndef DEBUG}{$apptype GUI}{$endif}

uses
  ARGBImagePNGReaderUnit,
  FNSMain;

const
  VERSION='0.9';
  BDATE={$i %DATE%};

begin
  with TMain.Create(VERSION, BDATE) do try
    Run;
  finally
    Free;
  end;
end.

