{******************************************************************

                       JEDI-VCL Demo

 Copyright (C) 2002 Project JEDI

 Original author: Ralf Grenzing [ralfgspam@gmx.de]
                  Uwe Rupprecht [uwe-rupprecht@gmx.de]

 Contributor(s): Michael Beck (mbeck1@compuserve.com)
 Settings part based on work of Angus Johnson - ajohnson@rpi.net.au

 You may retrieve the latest version of this file at the JEDI-JVCL
 home page, located at http://jvcl.sourceforge.net

 The contents of this file are used with permission, subject to
 the Mozilla Public License Version 1.1 (the "License"); you may
 not use this file except in compliance with the License. You may
 obtain a copy of the License at
 http://www.mozilla.org/MPL/MPL-1_1Final.html

 Software distributed under the License is distributed on an
 "AS IS" basis, WITHOUT WARRANTY OF ANY KIND, either express or
 implied. See the License for the specific language governing
 rights and limitations under the License.

******************************************************************}

unit hshape;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, Buttons, StdCtrls,
  JvgShape, JvgHoleShape, JvgBevel, JvComponent, JvExControls;

type
  THShapeFrm = class(TForm)
    FreeHoleShape2: TJvgHoleShape;
    FreeHoleShape3: TJvgHoleShape;
    FreeHoleShape4: TJvgHoleShape;
    FreeHoleShape5: TJvgHoleShape;
    FreeHoleShape6: TJvgHoleShape;
    FreeHoleShape10: TJvgHoleShape;
    FreeHoleShape7: TJvgHoleShape;
    FreeHoleShape9: TJvgHoleShape;
    FreeHoleShape11: TJvgHoleShape;
    FreeHoleShape12: TJvgHoleShape;
    FreeHoleShape14: TJvgHoleShape;
    FreeHoleShape15: TJvgHoleShape;
    FreeHoleShape16: TJvgHoleShape;
    glHoleShape1: TJvgHoleShape;
    glHoleShape3: TJvgHoleShape;
    FreeHoleShape13: TJvgHoleShape;
    glHoleShape2: TJvgHoleShape;
    glHoleShape4: TJvgHoleShape;
    glHoleShape5: TJvgHoleShape;
    glBevel1: TJvgBevel;
  private
  public
  end;

var
  HShapeFrm: THShapeFrm;

implementation

{$R *.dfm}

end.

