{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvId3v2Types.PAS, released on 2001-02-28.

The Initial Developer of the Original Code is Sébastien Buysse [sbuysse@buypin.com]
Portions created by Sébastien Buysse are Copyright (C) 2001 Sébastien Buysse.
All Rights Reserved.

Contributor(s): Michael Beck [mbeck@bigfoot.com].

Last Modified: 2000-02-28

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}

{$I JVCL.INC}

unit JvId3v2Types;

{
Not yet coded (really complicated)
 USLT  4.9
 SYLT  4.10
 COMM  4.11
 RVAD  4.12
 EQUA  4.13
 RVRB  4.14
 GEOB  4.16
 RJvF  4.19
 AENC  4.20
 POSS  4.22
 USER  4.23
 COMR  4.25
Total : 12

ignored:
 SYTC
 ENCR
 GRID
 PRIV
 LINK
 MLLT
}

{
 TJvId3v2
}



interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, extctrls;

type
  TId3v2Header = packed record
    Identifier: array[0..2] of Char;
    Version: Word;
    Flags: Byte;
    Size: Cardinal;
  end;
{$EXTERNALSYM TId3v2Header}

  TId3v2Frame = packed record
    Id: array[0..3] of Char;
    Size: Integer;
    Flags: Word;
  end;
{$EXTERNALSYM TId3v2Frame}

implementation

end.
