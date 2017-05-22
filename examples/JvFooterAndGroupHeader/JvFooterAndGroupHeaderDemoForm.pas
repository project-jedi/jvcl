{******************************************************************

                       JEDI-VCL Demo

 Copyright (C) 2002 Project JEDI

 Original author:

 Contributor(s):

 You may retrieve the latest version of this file at the JEDI-JVCL
 home page, located at http://jvcl.delphi-jedi.org

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

unit JvFooterAndGroupHeaderDemoForm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, StdCtrls, JvFooter, JvGroupHeader, JvComponent, JvBitBtn, Buttons,
  JvExButtons, JvExExtCtrls, JvExControls, JvExStdCtrls, JvButton, JvCtrls;

type
  TJvFooterAndGroupHeaderDemoFrm = class(TForm)
    JvFooter1: TJvFooter;
    JvFooterBtn1: TJvFooterBtn;
    JvFooterBtn2: TJvFooterBtn;
    JvFooterBtn3: TJvFooterBtn;
    JvFooter2: TJvFooter;
    JvFooter3: TJvFooter;
    JvFooterBtn4: TJvFooterBtn;
    JvFooterBtn5: TJvFooterBtn;
    JvFooterBtn6: TJvFooterBtn;
    JvFooterBtn7: TJvFooterBtn;
    JvGroupHeader1: TJvGroupHeader;
    JvGroupHeader2: TJvGroupHeader;
  end;

var
  JvFooterAndGroupHeaderDemoFrm: TJvFooterAndGroupHeaderDemoFrm;

implementation

{$R *.DFM}

end.