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

unit fBalls;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, JvMTThreading, StdCtrls, JvMTSync, JvMTData, JvMTConsts,
  ExtCtrls, ComCtrls, TeeProcs, TeEngine, Chart,
  JvMtComponents, JvComponent;

type
  TBallMove = class
    Ticket: TMTTicket;
    X,Y: Integer;
  end;

type
  TfBouncingBalls = class(TForm)
    Button1: TButton;
    Memo1: TMemo;
    ScrollBox: TScrollBox;
    CritShape: TShape;
    Button2: TButton;
    ThreadManager: TJvMtManager;
    BallThread: TJvMtThread;
    Buffer: TJvMtThreadToVCL;
    Section: TJvMtCountingSection;
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure JvMtThreadToVCL1CanRead(Sender: TObject);
    procedure BallThreadExecute(Sender: TJvMtThread;
      MTThread: TJvMtSingleThread);
    procedure BallThreadFinished(Sender: TJvMtThread;
      MTThread: TJvMtSingleThread);

  private
    { Private declarations }
    function PointInsideCritical(const X,Y: Extended): Boolean;
    procedure BounceBall;

    function  FindBall(Ticket: TMTTicket): TShape;
    procedure AddBall(Ticket: TMTTicket);
    procedure MoveBall(AMove: TBallMove);
    procedure RemoveBall(Ticket: TMTTicket);
  public
    { Public declarations }
  end;

var
  fBouncingBalls: TfBouncingBalls;

implementation

{$R *.dfm}

procedure TfBouncingBalls.FormCreate(Sender: TObject);
begin
  Randomize;
end;

procedure TfBouncingBalls.BounceBall;
var
  M: TBallMove;
  AntiSpeed: Integer;
  X,Y: Extended;
  DX,DY: Extended;
  InsideCrit: Boolean;
begin
  AntiSpeed := 10+Random(90);
  X := 32;
  Y := 32;
  repeat DX := 10*Random; until abs(DX)>=1;
  repeat DY := 10*Random; until abs(DY)>=1;
  InsideCrit := False;

  try
    while True do
    begin
      CurrentMTThread.CheckTerminate;
      Sleep(AntiSpeed);

      M := TBallMove.Create;
      M.Ticket := CurrentMTThread.Ticket;
      M.X := Round(X);
      M.Y := Round(Y);
      Buffer.Write(M);

      if (X+DX<32) or (X+DX>ScrollBox.Width-32) then DX := -DX;
      if (Y+DY<32) or (Y+DY>ScrollBox.Height-32) then DY := -DY;

      X := X+DX;
      Y := Y+DY;

      if (not InsideCrit) and PointInsideCritical(X,Y) then
      begin
        Section.Enter;
        InsideCrit := True;
      end
      else if InsideCrit and (not PointInsideCritical(X,Y)) then
      begin
        InsideCrit := False;
        Section.Leave;
      end;

    end;
  finally
    if InsideCrit then
      Section.Leave;
  end;
end;

procedure TfBouncingBalls.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  OutputDebugString('FORM CLOSE');
end;

procedure TfBouncingBalls.AddBall(Ticket: TMTTicket);
var
  Ball: TShape;
begin
  Ball := TShape.Create(ScrollBox);
  Ball.Tag := Ticket;
  Ball.Width := 16;
  Ball.Height := 16;
  Ball.Shape := stEllipse;
  Ball.Left := 0;
  Ball.Top := 0;
  Ball.Parent := ScrollBox;
  Ball.Visible := True;
end;

procedure TfBouncingBalls.MoveBall(AMove: TBallMove);
var
  Ball: TShape;
begin
  Ball := FindBall(AMove.Ticket);
  if Ball <> nil then
  begin
    Ball.Top := AMove.Y-8;
    Ball.Left := AMove.X-8;
  end;
end;

function TfBouncingBalls.FindBall(Ticket: TMTTicket): TShape;
var
  I: Integer;
begin
  I := Scrollbox.ComponentCount-1;
  while (I <> -1) and (ScrollBox.Components[I].Tag <> Ticket) do Dec(I);

  if I <> -1 then
    Result := TShape(ScrollBox.Components[I])
  else
    Result := nil;
end;

procedure TfBouncingBalls.RemoveBall(Ticket: TMTTicket);
begin
  FindBall(Ticket).Free;
end;


procedure TfBouncingBalls.Button1Click(Sender: TObject);
begin
  BallThread.RunCopy;
  AddBall(BallThread.Ticket);
  Memo1.Lines.Add('Starting ball with ticket '+IntToStr(BallThread.Ticket));
end;

procedure TfBouncingBalls.Button2Click(Sender: TObject);
begin
  ThreadManager.TerminateThreads;
end;

function TfBouncingBalls.PointInsideCritical(const X, Y: Extended): Boolean;
begin
  if CritShape.Shape = stEllipse then
    Result := PointInEllipse(Point(Round(X),Round(Y)), CritShape.BoundsRect)
  else if CritShape.Shape = stRectangle then
    Result := ((Y>CritShape.Top) and (Y<CritShape.Top+CritShape.Height)) and
      ((X>CritShape.Left) and (X<CritShape.Left+CritShape.Width))
  else
    Result := False;
end;

procedure TfBouncingBalls.FormDestroy(Sender: TObject);
begin
  OutputDebugString('FORM DESTROY');
end;

procedure TfBouncingBalls.JvMtThreadToVCL1CanRead(Sender: TObject);
var
  M: TBallMove;
begin
  M := Buffer.Read as TBallMove;
  try
    MoveBall(M);
  finally
    M.Free;
  end;
end;

procedure TfBouncingBalls.BallThreadExecute(Sender: TJvMtThread;
  MTThread: TJvMtSingleThread);
begin
  BounceBall;
end;

procedure TfBouncingBalls.BallThreadFinished(Sender: TJvMtThread;
  MTThread: TJvMtSingleThread);
begin
  OutputDebugString('FINISH BALL');
  Memo1.Lines.Add('Ending ball with ticket '+IntToStr(MTThread.Ticket));
  RemoveBall(MTThread.Ticket);
end;

end.
