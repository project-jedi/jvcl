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

unit LinkedConsumersMain;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  JvComponent, JvLabel, StdCtrls, JvCombobox, JvColorProvider,
  JvDataProvider, JvExStdCtrls, JvExControls;

type
  TForm2 = class(TForm)
    dpColor: TJvColorProvider;
    dpColorMapping: TJvColorMappingProvider;
    lblMappingCaption: TLabel;
    cbMapping: TJvComboBox;
    lblColorCaptions: TLabel;
    lblColor1: TJvLabel;
    lblColor2: TJvLabel;
    lblColor3: TJvLabel;
    lblColor4: TJvLabel;
    lblColor5: TJvLabel;
    lblColor6: TJvLabel;
    lblColor7: TJvLabel;
    lblColor8: TJvLabel;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form2: TForm2;

implementation

{$R *.DFM}

end.
