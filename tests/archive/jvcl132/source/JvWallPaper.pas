{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvWallPaper.PAS, released on 2001-02-28.

The Initial Developer of the Original Code is Sébastien Buysse [sbuysse@buypin.com]
Portions created by Sébastien Buysse are Copyright (C) 2001 Sébastien Buysse.
All Rights Reserved.

Contributor(s): Michael Beck [mbeck@bigfoot.com].

Last Modified: 2000-02-28

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
{$A+,B-,C+,D+,E-,F-,G+,H+,I+,J+,K-,L+,M-,N+,O+,P+,Q-,R-,S-,T-,U-,V+,W-,X+,Y+,Z1}
{$I JEDI.INC}

unit JvWallPaper;

{$OBJEXPORTALL On}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Registry, JvTypes, JvComponent;

type
  TJvWallpaper = class(TJvComponent)
  public
    procedure SetWallpaper(Path: string); overload;
    procedure SetWallpaper(Path: string; Style: TWallpaperStyle); overload;
  end;

implementation

resourcestring
  RC_ControlRegistry = 'Control Panel\Desktop';
  RC_WallpaperStyle = 'WallpaperStyle';
  RC_WallpaperRegistry = 'Wallpaper';
  RC_TileWallpaper = 'TileWallpaper';

  {************************************************************}

procedure TJvWallpaper.SetWallpaper(Path: string);
begin
  SystemParametersInfo(SPI_SETDESKWALLPAPER, 0, PChar(Path), SPIF_UPDATEINIFILE);
end;

{************************************************************}

procedure TJvWallpaper.SetWallpaper(Path: string; Style: TWallpaperStyle);
begin
  with TRegistry.Create do
  begin
    OpenKey(RC_ControlRegistry, False);
    case Style of
      wpTile:
        begin
          WriteString(RC_TileWallpaper, '1');
          WriteString(RC_Wallpaperstyle, '0');
        end;
      wpCenter:
        begin
          WriteString(RC_TileWallpaper, '0');
          WriteString(RC_Wallpaperstyle, '0');
        end;
      wpStretch:
        begin
          WriteString(RC_TileWallpaper, '0');
          WriteString(RC_Wallpaperstyle, '2');
        end;
    end;
    WriteString(RC_WallpaperRegistry, Path);
    Free;
  end;
  SystemParametersInfo(SPI_SETDESKWALLPAPER, 0, nil, SPIF_SENDWININICHANGE);
end;

end.
