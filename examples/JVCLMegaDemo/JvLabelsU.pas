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

unit JvLabelsU;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, JvBehaviorLabel, JvLabel;

type
  TJvLabelsFrm = class(TForm)
    JvHotLink1: TJvLabel;
    JvBlinkingLabel1: TJvBehaviorLabel;
    JvScrollingLabel1: TJvBehaviorLabel;
    JvRealLabel1: TJvBehaviorLabel;
    JvSpecialLabel1: TJvBehaviorLabel;
    JvAngleLabel1: TJvLabel;
    JvBouncingLabel1: TJvBehaviorLabel;
    JvAppearingLabel1: TJvBehaviorLabel;
    Label11: TLabel;
    JvReversedLabel1: TJvLabel;
    Label12: TLabel;
    procedure JvBouncingLabel1Click(Sender: TObject);
    procedure JvAppearingLabel1Click(Sender: TObject);
    procedure JvAppearingLabel1Appeared(Sender: TObject);
  end;

implementation

{$R *.dfm}

procedure TJvLabelsFrm.JvBouncingLabel1Click(Sender: TObject);
begin
  JvBouncingLabel1.BehaviorOptions.Active := true;
end;

procedure TJvLabelsFrm.JvAppearingLabel1Click(Sender: TObject);
begin
  JvAppearingLabel1.BehaviorOptions.Active := true;
end;

procedure TJvLabelsFrm.JvAppearingLabel1Appeared(Sender: TObject);
begin
 ShowMessage('I have appeared. Are you now satisfied?');
end;

end.
