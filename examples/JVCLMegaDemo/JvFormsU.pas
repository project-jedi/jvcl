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

unit JvFormsU;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Buttons, ExtCtrls, JvComponent, JvCaptionPanel;

type
  TJvFormsFrm = class(TForm)
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    BitBtn3: TBitBtn;
    BitBtn4: TBitBtn;
    BitBtn5: TBitBtn;
    BitBtn6: TBitBtn;
    BitBtn7: TBitBtn;
    BitBtn8: TBitBtn;
    BitBtn9: TBitBtn;
    BitBtn10: TBitBtn;
    BitBtn11: TBitBtn;
    BitBtn12: TBitBtn;
    procedure BitBtn1Click(Sender: TObject);
    procedure BitBtn2Click(Sender: TObject);
    procedure BitBtn3Click(Sender: TObject);
    procedure BitBtn4Click(Sender: TObject);
    procedure BitBtn5Click(Sender: TObject);
    procedure BitBtn6Click(Sender: TObject);
    procedure BitBtn7Click(Sender: TObject);
    procedure BitBtn8Click(Sender: TObject);
    procedure BitBtn9Click(Sender: TObject);
    procedure BitBtn10Click(Sender: TObject);
    procedure BitBtn11Click(Sender: TObject);
    procedure BitBtn12Click(Sender: TObject);
  public
    TheForm : TForm;
  end;

implementation

uses
  jvWallpaperform,
  jvAnimatedTitelform,
  jvPerforatedform,
  jvTransparentFormd,
  jvTrayicondemo,
  jvanimatedformicondemo,
  jvanimatedappicondemo,
  jvFormplacedemo,
  jvFormanimationdemo,
  jvAutosizeformdemo,
  jvMagnetformdemo,
  jvGradientformdemo;

{$R *.dfm}

procedure TJvFormsFrm.BitBtn1Click(Sender: TObject);
begin
  TheForm := TfrWallpaper.Create(nil);
  TheForm.showmodal;
  theForm.free;
end;

procedure TJvFormsFrm.BitBtn2Click(Sender: TObject);
begin
  TheForm := TfrAnimatedTitel.create(nil);
  theform.showmodal;
  theform.free;
end;

procedure TJvFormsFrm.BitBtn3Click(Sender: TObject);
begin
  TheForm := TfrPerforatedForm.create(nil);
  theform.showmodal;
  theform.free;
end;

procedure TJvFormsFrm.BitBtn4Click(Sender: TObject);
begin
  TheForm := TfrTransparentForm.create(nil);
  theform.showmodal;
  theform.free;
end;

procedure TJvFormsFrm.BitBtn5Click(Sender: TObject);
begin
  TheForm := TfrTrayicon.create(nil);
  theform.showmodal;
  theform.free;
end;

procedure TJvFormsFrm.BitBtn6Click(Sender: TObject);
begin
  TheForm := TfrAnimatedFormIcon.create(nil);
  theform.showmodal;
  theform.free;
end;

procedure TJvFormsFrm.BitBtn7Click(Sender: TObject);
begin
  TheForm := TfrAnimatedApplicationicon.create(nil);
  theform.showmodal;
  theform.free;
end;

procedure TJvFormsFrm.BitBtn8Click(Sender: TObject);
begin
  TheForm := TfrFormplace.create(nil);
  theform.showmodal;
  theform.free;
end;

procedure TJvFormsFrm.BitBtn9Click(Sender: TObject);
begin
  TheForm := TfrFormAnimation.create(nil);
  theform.showmodal;
  theform.free;
end;

procedure TJvFormsFrm.BitBtn10Click(Sender: TObject);
begin
  TheForm := TfrAutosize.create(nil);
  theform.showmodal;
  theform.free;
end;

procedure TJvFormsFrm.BitBtn11Click(Sender: TObject);
begin
  TheForm := Tfrmagnet.create(nil);
  theform.showmodal;
  theform.free;
end;

procedure TJvFormsFrm.BitBtn12Click(Sender: TObject);
begin
  TheForm := Tfrgradient.create(nil);
  theform.showmodal;
  theform.free;
end;

end.
