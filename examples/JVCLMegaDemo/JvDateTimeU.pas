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

unit JvDateTimeU;

interface

uses 
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  JvComponent, JvCalendar, ComCtrls, JvMonthCalendar, JvDateTimePicker,
  StdCtrls, JvEdit, JvValidateEdit, Mask, JvToolEdit, ExtCtrls,
  JvClock, JvExExtCtrls, JvExMask, JvExStdCtrls, JvExComCtrls;

type
  TJvDateTimeFrm = class(TForm)
    Label2: TLabel;
    Label3: TLabel;
    JvDateTimePicker1: TJvDateTimePicker;
    JvMonthCalendar1: TJvMonthCalendar;
    JvYearEdit1: TJvValidateEdit;
    Label1: TLabel;
    JvDateEdit1: TJvDateEdit;
    Label4: TLabel;
    Label6: TLabel;
    Label5: TLabel;
    JvxClock1: TJvClock;
    JvClock1: TJvClock;
  end;

implementation

{$R *.dfm}

end.
