{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvgTransparentMemo.PAS, released on 2003-01-15.

The Initial Developer of the Original Code is Andrey V. Chudin,  [chudin att yandex dott ru]
Portions created by Andrey V. Chudin are Copyright (C) 2003 Andrey V. Chudin.
All Rights Reserved.

Contributor(s):
Michael Beck [mbeck att bigfoot dott com].

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit JvgTransparentMemo;

{$I jvcl.inc}
{$I windowsonly.inc}

interface

uses
  {$IFDEF USEJVCL}
  Windows, Messages, SysUtils, Classes, Controls, Forms, StdCtrls,
  JVCLVer;
  {$ELSE}
  Windows, Messages, SysUtils, Classes, Controls, Forms, StdCtrls;
  {$ENDIF USEJVCL}

type
  TJvgTransparentMemo = class(TMemo)
  private
    {$IFDEF USEJVCL}
    FAboutJVCL: TJVCLAboutInfo;
    {$ENDIF USEJVCL}
    procedure WMPaint(var Msg: TWMPaint); message WM_PAINT;
    procedure WMEraseBkgnd(var Msg: TWMEraseBkgnd); message WM_ERASEBKGND;
  public
    procedure CreateParams(var Params: TCreateParams); override;
  published
    {$IFDEF USEJVCL}
    property AboutJVCL: TJVCLAboutInfo read FAboutJVCL write FAboutJVCL stored False;
    {$ENDIF USEJVCL}
  end;

implementation

procedure TJvgTransparentMemo.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  Params.ExStyle := Params.ExStyle or WS_EX_TRANSPARENT;
end;

procedure TJvgTransparentMemo.WMPaint(var Msg: TWMPaint);
var
  DC: HDC;
begin
  DC := GetDC(Handle);
  SetBkMode(DC, Windows.TRANSPARENT);
  ReleaseDC(Handle, DC);
  inherited;
end;

procedure TJvgTransparentMemo.WMEraseBkgnd(var Msg: TWMEraseBkgnd);
begin
end;

end.

