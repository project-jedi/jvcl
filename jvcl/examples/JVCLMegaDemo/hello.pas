{******************************************************************
                                                                  
                       JEDI-VCL Demo

 Copyright (C) 2002 Project JEDI

 Original author: Ralf Grenzing [Ralf dot Grenzing@gmx.de]
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

unit hello;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, JvLabel, JvScrollText, jpeg, ExtCtrls, JvImage,
  JvComponent, JvBaseDlg, JvJVCLAboutForm, JvExControls,
  JvExExtCtrls, JvExStdCtrls, JvRichEdit;

type
  TWelcomeForm = class(TForm)
    frmh_st: TJvScrollText;
    JvImage1: TJvImage;
    JvHotLink2: TJvLabel;
    JvJVCLAboutComp: TJvJVCLAboutComponent;
    JvHotLink3: TJvLabel;
    JvRichEdit1: TJvRichEdit;
    procedure FormShow(Sender: TObject);
    procedure JvHotLink3Click(Sender: TObject);
  end;

implementation

{$R *.dfm}

procedure TWelcomeForm.FormShow(Sender: TObject);
begin
 //Start the scrolling of the ScollText comp
 frmh_st.active := true;
 self.SetFocus; // Hide cursor from RichEdit
end;

procedure TWelcomeForm.JvHotLink3Click(Sender: TObject);
begin
 JvJVCLAboutComp.Execute;
end;

end.
