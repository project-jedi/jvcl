{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvJVCLAbout.PAS, released on 2001-02-28.

The Initial Developer of the Original Code is Michael Beck [mbeck@bigfoot.com]
Portions created by Michael Beck are Copyright (C) 2002 Michael Beck
All Rights Reserved.

Contributor(s):

Last Modified: 2000-02-28

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
{$A+,B-,C+,D+,E-,F-,G+,H+,I+,J+,K-,L+,M-,N+,O+,P+,Q-,R-,S-,T-,U-,V+,W-,X+,Y+,Z1}
{$I JEDI.INC}

unit JvJVCLAbout;

interface

uses
  Windows, Messages, SysUtils, {$IFDEF DELPHI6_UP}Variants, {$ENDIF}
  Classes, Graphics, Controls, Forms,
  Dialogs, JvHotLink, StdCtrls, ExtCtrls, jpeg, JvLabel,
{$IFDEF DELPHI5}DsgnIntf, {$ENDIF}{$IFDEF DELPHI6_UP}DesignEditors, DesignIntf, {$ENDIF}
  JVCLVer, JvComponent, JvExecute, Buttons, JvBitBtn,IniFiles ;

type

  TJVCLAboutDialogProperty = class(TPropertyEditor)
  private
  public
    procedure Edit; override;
    function GetAttributes: TPropertyAttributes; override;
    function GetValue: string; override;
  end;

  TJvJVCLAboutForm = class(TForm)
    Bevel1: TBevel;
    lblVersion: TLabel;
    pnlImage: TPanel;
    Image1: TImage;
    btnOK: TButton;
    JvHotLink1: TJvHotLink;
    JvHotLink4: TJvHotLink;
    lblNews: TJvHotLink;
    Label1: TLabel;
    Label2: TLabel;
    lblCopyRight: TLabel;
    lblRights: TLabel;
    Image3: TImage;
    MainPanel: TPanel;
    Bevel2: TBevel;
    lblVisitJedi: TLabel;
    lblMailingList: TLabel;
    lblNewsgroup: TLabel;
    lblJvHotLink2: TJvHotLink;
    lblBugs: TLabel;
    lblBugsURL: TJvHotLink;
    JvExecute1: TJvExecute;
    btnHelp: TSpeedButton;
    btnOptions: TSpeedButton;
    OpenDialog1: TOpenDialog;
    procedure btnOKClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure Panel1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure btnHelpClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure btnOptionsClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  protected
    procedure CreateParams(var Params: TCreateParams); override;

  private
    { Private declarations }
    FHelpFile:String;
    FHelpDirectory:String;
    procedure LoadOptions;
    procedure SaveOptions;
  public
    { Public declarations }
  end;

var
  JvJVCLAboutForm: TJvJVCLAboutForm;

implementation

{$R *.dfm}

{ TJVCLAboutDialogProperty }

procedure TJVCLAboutDialogProperty.Edit;
var
  Dialog: TJvJVCLAboutForm;
begin
  Dialog := TJvJVCLAboutForm.Create(nil);
  try
    Dialog.ShowModal;
  finally
    Dialog.Free;
  end;

end;

function TJVCLAboutDialogProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paDialog, paReadOnly];
end;

function TJVCLAboutDialogProperty.GetValue: string;
begin
  Result := 'Version ' + JVCL_VERSIONSTRING;
end;


{ TJvJVCLAboutForm }

procedure TJvJVCLAboutForm.btnOKClick(Sender: TObject);
begin
  Close;
end;

procedure TJvJVCLAboutForm.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  with Params do
    begin
      Style := (Style or WS_POPUP) and (not WS_DLGFRAME);
    end;
end;

procedure TJvJVCLAboutForm.FormShow(Sender: TObject);
begin
  lblVersion.Caption := 'Version: '+JVCL_VERSIONSTRING;
  lblCopyRight.Caption := 'Copyright© Project JEDI, 1999 - ' + FormatDateTime('yyyy', Now);
end;

procedure TJvJVCLAboutForm.Panel1MouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  ReleaseCapture;
  Perform(WM_SYSCOMMAND, SC_MOVE + 2, 0);
end;

procedure TJvJVCLAboutForm.btnHelpClick(Sender: TObject);
begin
  JvExecute1.Exec(FHelpFile, '', FHelpDirectory);
  Close;
end;

procedure TJvJVCLAboutForm.FormCreate(Sender: TObject);
begin
  LoadOptions;
  if FHelpFile='' then btnHelp.Enabled:=False else btnHelp.Enabled := True;
end;

procedure TJvJVCLAboutForm.btnOptionsClick(Sender: TObject);
begin

  if OpenDialog1.Execute then
    begin
      FHelpFile:= ExtractFileName(OpenDialog1.FileName);
      FHelpDirectory:= ExtractFileDir(OpenDialog1.FileName);
      SaveOptions;
    if FHelpFile='' then btnHelp.Enabled:=False else btnHelp.Enabled := True;
    end;
end;

procedure TJvJVCLAboutForm.LoadOptions;
var
  l, t, w, h: integer;
begin
  with TIniFile.create(ExtractFileDir(Application.exename)+'\JVCL.ini') do
    try
      l := ReadInteger('Options', 'Bounds.Left', 0);
      t := ReadInteger('Options', 'Bounds.Top', 0);
      w := ReadInteger('Options', 'Bounds.Width', -1);
      h := ReadInteger('Options', 'Bounds.Height', -1);

      FHelpFile := ReadString('Options', 'Help.File', '');
      FHelpDirectory := ReadString('Options', 'Help.Directory', '');
    finally
      free;
    end;

  //make sure the form is positioned on screen ...
  //(ie make sure nobody's fiddled with the INI file!)
  if (w > 0) and (h > 0) and
    (l < screen.Width) and (t < screen.Height) and
    (l + w > 0) and (t + h > 0) then
    setbounds(l, t, w, h);
end;

procedure TJvJVCLAboutForm.SaveOptions;
begin
  with TIniFile.create(ExtractFileDir(Application.exename)+'\JVCL.ini') do
    try
      if windowState = wsNormal then
        begin
          WriteInteger('Options', 'Bounds.Left', self.Left);
          WriteInteger('Options', 'Bounds.Top', self.Top);
          WriteInteger('Options', 'Bounds.Width', self.Width);
          WriteInteger('Options', 'Bounds.Height', self.Height);
        end;

      WriteString('Options', 'Help.File', FHelpFile);
      WriteString('Options', 'Help.Directory', FHelpDirectory);
    finally
      free;
    end;
end;

procedure TJvJVCLAboutForm.FormDestroy(Sender: TObject);
begin
SaveOptions;
end;

end.

