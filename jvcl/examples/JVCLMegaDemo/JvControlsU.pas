{******************************************************************

                       JEDI-VCL Demo

 Copyright (C) 2002 Project JEDI

 Original author:

 Contributor(s):

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

unit JvControlsU;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, JvButton, JvFavoritesButton, 
  JvClock, JvCombobox, JvGammaPanel, JvGradientCaption,
  Spin, JvZoom, JvWaitingGradient, ExtCtrls, JvShape, JvListComb, ImgList, JvComponent, JvTimeLine, JvListBox, JvCtrls,
  JvExControls, JvExStdCtrls;

type
  TJvControls = class(TForm)
    JvGradientCaption1: TJvGradientCaption;
    JvWaitingGradient1: TJvWaitingGradient;
    Label3: TLabel;
    Label4: TLabel;
    ImageList1: TImageList;
    JvImageListBox1: TJvImageListBox;
    JvMultilineListbox1: TJvListBox;
    JvTimeLine1: TJvTimeLine;
  end;

implementation

{$R *.dfm}

end.
