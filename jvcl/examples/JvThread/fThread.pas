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

unit fThread;

{$I jvcl.inc}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, JvThread, JvComponent, JvThreadDialog, ComCtrls, //JvThreadGifDialog,
  JvExControls, JvAnimatedImage, JvGIFCtrl, JvProgressBar, JvExComCtrls,
  JvComponentBase, ActnList, ExtCtrls;

type
  TForm1 = class(TForm)
    Label1: TLabel;
    JvThread1: TJvThread;
    JvThread2: TJvThread;
    Button1: TButton;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Button2: TButton;
    JvThreadSimpleDialog1: TJvThreadSimpleDialog;
    JvThreadAnimateDialog1: TJvThreadAnimateDialog;
    Button3: TButton;
    Button4: TButton;
    Button5: TButton;
    procedure JvThread1Execute(Sender: TObject; params: Pointer);
    procedure Button1Click(Sender: TObject);
    procedure JvThread2Execute(Sender: TObject; params: Pointer);
    procedure Button2Click(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure ThreadDialogChangeThreadDialogOptions(DialogOptions: TJvThreadBaseDialogOptions);
  private
    ThreadInfoProgressBarPosition: Integer;
    ThreadInfoText: string;
    procedure OnThreadExecute(Sender: TObject; Params: Pointer);
  public
    Value: Integer;
    Value2: Integer;
    procedure Stats1;
    procedure Stats2;
  end;

var
  Form1: TForm1;

implementation

uses
  JvDynControlEngineVcl
  {$IFDEF USE_3RDPARTY_DEVEXPRESS_CXEDITOR}
  ,JvDynControlEngineDevExpcx
  {$ENDIF}
  ;


{$R *.dfm}

procedure TForm1.JvThread1Execute(Sender: TObject; params: Pointer);
var
  i, j, k: Integer;
begin
  //Do the job here
  k := 0;
  for i := 0 to 100 do
    for j := 0 to 800 do
    begin
      Inc(k);

      //To use global variable/objects, you have to synchronize (to avoid conflicts)
      TForm1(params).Value := k;
      JvThread1.Synchronize(TForm1(params).Stats1);

      if JvThread1.Terminated then
        Exit;
    end;
end;

procedure TForm1.JvThread2Execute(Sender: TObject; params: Pointer);
var
  i, j, k: Integer;
begin
  //Do the job here
  k := 0;
  for i := 0 to 100 do
    for j := 0 to 10 do
    begin
      Inc(k,5); //This is the only difference with the other thread
      sleep(13);
      //To use global variable/objects, you have to synchronize (to avoid conflicts)
      TForm1(params).Value2 := k;
      JvThread2.Synchronize(TForm1(params).Stats2);

      if JvThread2.Terminated then
        Exit;
    end;

end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  JvThread1.ThreadDialog := Nil;
  JvThread1.Execute(Self);
  (Sender as TButton).Enabled := False;
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  JvThread2.ThreadDialog := Nil;
  JvThread2.Execute(Self);
  (Sender as TButton).Enabled := False;
end;

procedure TForm1.Stats1;
begin
  Label2.Caption := IntToStr(Value);
end;

procedure TForm1.Stats2;
begin
  Label4.Caption := IntToStr(Value2);
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  if not JvThread1.Terminated then
    JvThread1.Terminate;
  if not JvThread2.Terminated then
    JvThread2.Terminate;
  while not (JvThread1.Terminated or JvThread2.Terminated) do
  begin
    Sleep(100);
    Application.ProcessMessages;
  end;

end;

procedure TForm1.Button4Click(Sender: TObject);
begin
  JvThread2.ThreadDialog := JvThreadAnimateDialog1;
  JvThread2.Execute(Self);
end;

procedure TForm1.Button3Click(Sender: TObject);
var
  Thread: TJvThread;
  ThreadDialog: TJvThreadSimpleDialog;
begin
  Thread := TJvThread.Create(Self);
  ThreadDialog := TJvThreadSimpleDialog.Create(Self);
  try
    Thread.Name := 'Thread';
    Thread.Exclusive := True;
    Thread.MaxCount := 0;
    Thread.RunOnCreate := True;
    Thread.FreeOnTerminate := True;
    Thread.ThreadDialog := ThreadDialog;
    Thread.OnExecute := OnThreadExecute;
    ThreadDialog.Name := 'ThreadDialog';
    Thread.ThreadDialog.DialogOptions.Assign(JvThreadSimpleDialog1.DialogOptions);
    ThreadDialog.ChangeThreadDialogOptions :=  ThreadDialogChangeThreadDialogOptions;
    ThreadDialog.DialogOptions.Caption := 'Sizing Thread Sample';
    Thread.ExecuteWithDialog(nil);
  finally
    Thread.Free;
    ThreadDialog.Free;
  end;

end;

procedure TForm1.ThreadDialogChangeThreadDialogOptions(DialogOptions: TJvThreadBaseDialogOptions);
begin
  DialogOptions.InfoText := ThreadInfoText;
  if DialogOptions is TJvThreadSimpleDialogOptions then
    TJvThreadSimpleDialogOptions(DialogOptions).ProgressbarPosition := ThreadInfoProgressBarPosition;
end;

procedure TForm1.OnThreadExecute(Sender: TObject; Params: Pointer);
var
  i: Integer;
begin
//  (TBaseSQLUtility(Params));
  for i := 0 to 20 do
  begin
    ThreadInfoProgressBarPosition := i;
    case i mod 5 of
      1 : ThreadInfoText := 'Das ist ein Test';
      2 : ThreadInfoText := 'ist ein Test';
      3 : ThreadInfoText := 'Das ist ein sehr sehr sehr langer Test der nicht zu umbrüchen führt';
      4 : ThreadInfoText := 'Das ist ein langer Test der nicht zu umbrüchen führt';
      5 : ThreadInfoText := 'Das ist ein langer Test der zu umbrüchen führt';
      else
        ThreadInfoText := 'Das ist ein anderer Test der auch zu lang wird und daher nicht passt';
    end;
    Sleep(1000);
  end;

end;

end.