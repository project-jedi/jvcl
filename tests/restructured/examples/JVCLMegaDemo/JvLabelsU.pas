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
  JvReversedLabel, JvAppearingLabel, JvBouncingLabel, JvAngleLabel,
  JvSpecialLabel, JvRealLabel,  JvScrollingLabel, JvBlinkingLabel,
  StdCtrls, JvLabel, JvHotLink;

type
  TJvLabelsFrm = class(TFrame)
    JvHotLink1: TJvHotLink;
    JvBlinkingLabel1: TJvBlinkingLabel;
    JvScrollingLabel1: TJvScrollingLabel;
    JvRealLabel1: TJvRealLabel;
    JvSpecialLabel1: TJvSpecialLabel;
    JvAngleLabel1: TJvAngleLabel;
    JvBouncingLabel1: TJvBouncingLabel;
    JvAppearingLabel1: TJvAppearingLabel;
    Label11: TLabel;
    JvReversedLabel1: TJvReversedLabel;
    Label12: TLabel;
    procedure JvBouncingLabel1Click(Sender: TObject);
    procedure JvAppearingLabel1Click(Sender: TObject);
    procedure JvAppearingLabel1Appeared(Sender: TObject);
  private
    { Private-Deklarationen }
  public
    { Public-Deklarationen }
  end;

implementation

{$R *.DFM}

procedure TJvLabelsFrm.JvBouncingLabel1Click(Sender: TObject);
begin
 JvBouncingLabel1.Bouncing := true;
end;

procedure TJvLabelsFrm.JvAppearingLabel1Click(Sender: TObject);
begin
 JvAppearingLabel1.Appear;
end;

procedure TJvLabelsFrm.JvAppearingLabel1Appeared(Sender: TObject);
begin
 ShowMessage('I have appeared. Are you now satisfied?');
end;

end.
