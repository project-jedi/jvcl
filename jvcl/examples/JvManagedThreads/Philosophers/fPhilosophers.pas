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

unit fPhilosophers;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, JvMTThreading, ExtCtrls, ComCtrls,
  JvMtComponents, JvComponent;

type
  TPhilosopherState = (psThinking, psHungry, psEating);

  TPerson = class
  public
    Nr: Integer;
    constructor Create(ANumber: Integer);
  end;

  TMsg = class(TObject)
  public
    Msg: string;
    constructor Create(AMsg: string);
  end;

  TfrmDiningPhilosophers = class(TForm)
    PhilosopherManager: TJvMtManager;
    PhilosopherThread: TJvMtThread;
    MonitorSection: TJvMtMonitorSection;
    Memo: TMemo;
    BtnStart: TButton;
    BtnTerminate: TButton;
    PersonBuffer: TJvMtVCLToThread;
    MsgToVCL: TJvMtThreadToVCL;
    PhilLabel1: TLabel;
    PhilLabel2: TLabel;
    PhilLabel3: TLabel;
    PhilLabel4: TLabel;
    PhilLabel5: TLabel;
    Shape1: TShape;
    SpeedBar: TTrackBar;
    LblSpeed: TLabel;
    BtnClose: TButton;
    procedure BtnStartClick(Sender: TObject);
    procedure BtnTerminateClick(Sender: TObject);
    procedure PersonBufferCanWrite(Sender: TObject);
    procedure MsgToVCLCanRead(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure SpeedBarChange(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure BtnCloseClick(Sender: TObject);
    procedure PhilosopherThreadExecute(Sender: TJvMtThread;
      MTThread: TJvMtSingleThread);
  private
    { Private declarations }
    FSpeed: Integer;
    FNrCycle: Integer;
    FState: Array[0..4] of TPhilosopherState;
    procedure SetPhilLabelsEnabled(Value: Boolean);
    procedure TerminatePhilosophers;
    procedure ProcessMsg(M: string);
    procedure WaitRandom;
  public
    { Public declarations }
  end;

var
  frmDiningPhilosophers: TfrmDiningPhilosophers;

implementation

{$R *.dfm}

{ TPerson }

constructor TPerson.Create(ANumber: Integer);
begin
  Nr := ANumber;
end;

{ TMsg }

constructor TMsg.Create(AMsg: string);
begin
  Msg := AMsg;
end;

{ TForm2 }


procedure TfrmDiningPhilosophers.WaitRandom;
var
  I: Integer;
begin
  for I := 0 to 5+Random(15) do
  begin
    Sleep(FSpeed);
    CurrentMTThread.CheckTerminate;
  end;
end;

procedure TfrmDiningPhilosophers.BtnStartClick(Sender: TObject);
var
  I: Integer;
begin
  Memo.Lines.Add('Terminating previous philosophers...');
  TerminatePhilosophers;
  PhilosopherManager.WaitThreads;

  Memo.Lines.Add('Initiating five new philosophers...');

  // reset the states
  for I := 0 to 4 do
    FState[I] := psThinking;

  // make 5 philosophers
  for I := 0 to 4 do
    PhilosopherThread.RunCopy;

  SetPhilLabelsEnabled(True);
end;

procedure TfrmDiningPhilosophers.BtnTerminateClick(Sender: TObject);
begin
  Memo.Lines.Add('Terminating all philosophers...');
  TerminatePhilosophers;
end;

procedure TfrmDiningPhilosophers.PersonBufferCanWrite(Sender: TObject);
begin
  PersonBuffer.Write(TPerson.Create(FNrCycle));
  FNrCycle := (FNrCycle+1) mod 5;
end;

procedure TfrmDiningPhilosophers.ProcessMsg(M: string);
var
  Nr: Integer;
  Lbl: TLabel;
begin
  Nr := StrToInt(Copy(M,1,1))+1;
  Lbl:=FindComponent('PhilLabel'+IntToStr(Nr)) as TLabel;
  if Assigned(Lbl) then Lbl.Caption := Copy(M,2,255);
end;

procedure TfrmDiningPhilosophers.MsgToVCLCanRead(Sender: TObject);
var
  M: TMsg;
begin
  M := TMsg(MsgToVCL.Read);
  try
    ProcessMsg(M.Msg);
  finally
    M.Free;
  end;
end;

procedure TfrmDiningPhilosophers.FormCreate(Sender: TObject);
begin
  Randomize;
end;

procedure TfrmDiningPhilosophers.TerminatePhilosophers;
begin
  // terminate any philosphers
  PhilosopherManager.TerminateThreads;
  SetPhilLabelsEnabled(False);
end;

procedure TfrmDiningPhilosophers.SetPhilLabelsEnabled(Value: Boolean);
begin
  PhilLabel1.Enabled := Value;
  PhilLabel2.Enabled := Value;
  PhilLabel3.Enabled := Value;
  PhilLabel4.Enabled := Value;
  PhilLabel5.Enabled := Value;
end;

procedure TfrmDiningPhilosophers.SpeedBarChange(Sender: TObject);
begin
  FSpeed := SpeedBar.Position;
end;

procedure TfrmDiningPhilosophers.FormShow(Sender: TObject);
begin
  FSpeed := SpeedBar.Position;
end;

procedure TfrmDiningPhilosophers.BtnCloseClick(Sender: TObject);
begin
  Close;
end;

procedure TfrmDiningPhilosophers.PhilosopherThreadExecute(
  Sender: TJvMtThread; MTThread: TJvMtSingleThread);
  procedure Test(Nr: Integer);
  begin
    if (FState[(Nr+4) mod 5] <> psEating) and (FState[Nr] = psHungry) and
      (FState[(Nr+1) mod 5] <> psEating) then
    begin
      FState[Nr] := psEating;
      MonitorSection[Nr].Signal;
    end;
  end;

  procedure PickupChopsticks(Nr: Integer);
  begin
    MonitorSection.Enter;
    try
      FState[Nr] := psHungry;
      Test(Nr);
      if FState[Nr] <> psEating then
        MonitorSection[Nr].Wait;
    finally
      MonitorSection.Leave;
    end;
  end;

  procedure PutdownChopsticks(Nr: Integer);
  begin
    MonitorSection.Enter;
    try
      FState[Nr] := psThinking;
      Test((Nr+4) mod 5);
      Test((Nr+1) mod 5);
    finally
      MonitorSection.Leave;
    end;
  end;

  procedure Msg(S: string);
  begin
    //OutputDebugString(PChar(S));
    MsgToVCL.Write(TMsg.Create(S));
  end;

var
  Person: TPerson;

begin
  OutputDebugString(PChar('Philosopher is waiting for personality...'));
  Person := PersonBuffer.Read as TPerson;
  try
    Msg(IntToStr(Person.Nr)+' Acquired personality');

    while True do
    begin
      // philosopher is thinking
      Msg(IntToStr(Person.Nr)+' Thinking');
      WaitRandom;

      // philosopher is hungry
      Msg(IntToStr(Person.Nr)+' Pickup chopsticks');
      PickupChopsticks(Person.Nr);

      // philosopher is eating
      Msg(IntToStr(Person.Nr)+' Eating');
      WaitRandom;

      // philosopher is finished eating
      //Msg(IntToStr(Person.Nr)+' Putdown chopsticks');
      PutdownChopsticks(Person.Nr);
    end;

  finally
    Person.Free;
  end;
end;

end.
