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

unit JvShFileOperationMainFormU;

{$I jvcl.inc}

interface

uses       
  Windows, Messages, SysUtils,
  {$IFDEF COMPILER6_UP} Variants, {$ENDIF COMPILER6_UP}
  Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, JvComponent, JvBaseDlg, ExtCtrls, JvSHFileOperation;

type
  TJvShFileOperationMainForm = class(TForm)
    JvSHFileOperation1: TJvSHFileOperation;
    btnCopy: TButton;
    btnMove: TButton;
    btnRename: TButton;
    btnDelete: TButton;
    memSource: TMemo;
    JvLabel1: TLabel;
    JvLabel2: TLabel;
    memDest: TMemo;
    JvGroupBox1: TGroupBox;
    chkUndo: TCheckBox;
    chkFiles: TCheckBox;
    chkMulti: TCheckBox;
    chkNoConfirm: TCheckBox;
    chkNoDirCreate: TCheckBox;
    chkRename: TCheckBox;
    chkSilent: TCheckBox;
    chkSimple: TCheckBox;
    chkMappings: TCheckBox;
    chkNoErrors: TCheckBox;
    JvBevel1: TBevel;
    JvLabel3: TLabel;
    memMessages: TMemo;
    Label1: TLabel;
    edTitle: TEdit;
    chkNoSecAttrs: TCheckBox;
    chkNoRecurse: TCheckBox;
    chkNoConElem: TCheckBox;
    chkNoParse: TCheckBox;
    chkWantNukes: TCheckBox;
    procedure JvSHFileOperation1FileMapping(Sender: TObject;
      const OldFileName, NewFilename: String);
    procedure btnCopyClick(Sender: TObject);
    procedure btnMoveClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure btnRenameClick(Sender: TObject);
    procedure btnDeleteClick(Sender: TObject);
  private
    procedure DoIt(AType: TJvSHFileOpType;const OKMsg:string);
  end;

var
  JvShFileOperationMainForm: TJvShFileOperationMainForm;

implementation

{$R *.dfm}

procedure TJvShFileOperationMainForm.JvSHFileOperation1FileMapping(Sender: TObject;
  const OldFileName, NewFilename: String);
begin
  memMessages.Lines.Add(Format('"%s" renamed as "%s"',[OldFileName,NewFileName]));
end;

procedure TJvShFileOperationMainForm.DoIt(AType:TJvSHFileOpType;const OKMsg:string);
var AOptions:TJvShFileOptions;
begin
  memMessages.Lines.Clear;
  JvSHFileOperation1.Operation := AType;
  JvSHFileOperation1.SourceFiles := memSource.Lines;
  JvSHFileOperation1.DestFiles := memDest.Lines;
  JvSHFileOperation1.Title := edTitle.Text;
  AOptions := [];
  if chkUndo.Checked then
    Include(AOptions,fofAllowUndo);
  if chkFiles.Checked then
    Include(AOptions,fofFilesOnly);
  if chkMulti.Checked then
    Include(AOptions,fofMultiDestFiles);
  if chkNoConfirm.Checked then
    Include(AOptions,fofNoConfirmation);
  if chkNoDirCreate.Checked then
    Include(AOptions,fofNoConfirmMkDir);
  if chkRename.Checked then
    Include(AOptions,fofRenameOnCollision);
  if chkSilent.Checked then
    Include(AOptions,fofSilent);
  if chkSimple.Checked then
    Include(AOptions,fofSimpleProgress);
  if chkMappings.Checked then
    Include(AOptions,fofWantMappingHandle);
  if chkNoErrors.Checked then
    Include(AOptions,fofNoErrorUI);
  if chkNoSecAttrs.Checked then
    Include(AOptions,fofNoCopySecurityAttributes);
  if chkNoRecurse.Checked then
    Include(AOptions,fofNoRecursion);
  if chkNoConElem.Checked then
    Include(AOptions,fofNoConnectedElements);
  if chkNoParse.Checked then
    Include(AOptions,fofNoRecurseParse);
  if chkWantNukes.Checked then
    Include(AOptions,fofWantNukeWarning);
  JvSHFileOperation1.Options := AOptions;
  if not JvSHFileOperation1.Execute then
    memMessages.Lines.Add(SysErrorMessage(GetLastError))
  else
    memMessages.Lines.Add(OkMsg);
end;  

procedure TJvShFileOperationMainForm.btnCopyClick(Sender: TObject);
begin
  DoIt(foCopy,'Copy finished');
end;

procedure TJvShFileOperationMainForm.btnMoveClick(Sender: TObject);
begin
  DoIt(foMove,'Move finished');
end;

procedure TJvShFileOperationMainForm.btnRenameClick(Sender: TObject);
begin
  DoIt(foRename,'Rename finished');
end;

procedure TJvShFileOperationMainForm.btnDeleteClick(Sender: TObject);
begin
  DoIt(foDelete,'Delete finished');
end;

procedure TJvShFileOperationMainForm.FormCreate(Sender: TObject);
begin
  Application.HintHidePause := 5000;
end;

end.
