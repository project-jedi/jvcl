{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvgTransparentMemo.PAS, released on 2003-01-15.

The Initial Developer of the Original Code is Andrey V. Chudin,  [chudin@yandex.ru]
Portions created by Andrey V. Chudin are Copyright (C) 2003 Andrey V. Chudin.
All Rights Reserved.

Contributor(s):
Michael Beck [mbeck@bigfoot.com].

Last Modified:  2003-01-15

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}

{$I JVCL.INC}

unit JvgTransparentMemo;

interface

uses
   Windows,
   JVCLVer,
   Messages,
   SysUtils,
   Classes,
   Graphics,
   Controls,
   Forms,
   Dialogs,
   StdCtrls;

type
   TJvgTranspMemo = class(TMemo)
   private
    FAboutJVCL: TJVCLAboutInfo;
      procedure OnWMPaint(var Msg: TWMPaint); message WM_PAINT;
   protected
      { Protected declarations }
   public
      constructor Create(AOwner: TComponent); override;
      procedure CreateParams(var Params: TCreateParams); override;
   published
      { Published declarations }
    property AboutJVCL: TJVCLAboutInfo read FAboutJVCL write FAboutJVCL stored False;
   end;

procedure Register;

implementation

procedure Register;
begin
end;

constructor TJvgTranspMemo.Create(AOwner: TComponent);
begin
   inherited;
   //Canvas.Brush.Style:=bsClear;
end;

procedure TJvgTranspMemo.CreateParams(var Params: TCreateParams);
begin
   inherited CreateParams(Params);
   //if Transparent then
   Params.ExStyle := Params.ExStyle or WS_EX_Transparent;
end;

procedure TJvgTranspMemo.OnWMPaint(var Msg: TWMPaint);
var
   dc                         : HDC;
begin
   dc := GetDC(handle);
   SetBkMode(dc, TRANSPARENT);
   releaseDC(handle, dc);
   inherited;
end;

end.

