{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvThreadTimer.PAS, released on 2001-02-28.

The Initial Developer of the Original Code is Sébastien Buysse [sbuysse@buypin.com]
Portions created by Sébastien Buysse are Copyright (C) 2001 Sébastien Buysse.
All Rights Reserved.

Contributor(s): Michael Beck [mbeck@bigfoot.com].

Last Modified: 2000-02-28

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
{$A+,B-,C+,D+,E-,F-,G+,H+,I+,J+,K-,L+,M-,N+,O+,P+,Q-,R-,S-,T-,U-,V+,W-,X+,Y+,Z1}
{$I JEDI.INC}

unit JvThreadTimer;

{$OBJEXPORTALL On}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, JvTypes, JvComponent;

type
  TJvThreadTimerclass = class(TThread)
  protected
    procedure Call;
    procedure Execute; override;
  public
    FDelay: Cardinal;
    FOnCall: TNotifyEvent;
    FSender: TObject;
  end;

  TJvThreadTimer = class(TJvComponent)
  private
    FActive: Boolean;
    FDelay: Integer;
    FOnTimer: TNotifyEvent;
    FThread: TJvThreadTimerClass;
    procedure SetActive(const Value: Boolean);
    procedure SetDelay(const Value: Integer);
    procedure SetOnTimer(const Value: TNotifyEvent);
  protected
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Active: Boolean read FActive write SetActive default False;
    property Delay: Integer read FDelay write SetDelay default 100;
    property OnTimer: TNotifyEvent read FOnTimer write SetOnTimer;
  end;

implementation

///////////////////////////////////////////////////////////
// TJvThreadTimerClass
///////////////////////////////////////////////////////////

{**************************************************}

procedure TJvThreadTimerClass.Call;
begin
  if Assigned(FOnCall) then
    FOnCall(FSender);
end;

{**************************************************}

procedure TJvThreadTimerClass.Execute;
begin
  while not Terminated do
  begin
    Sleep(FDelay);
    Synchronize(Call);
  end;
end;

///////////////////////////////////////////////////////////
// TJvThreadTimer
///////////////////////////////////////////////////////////

constructor TJvThreadTimer.Create(AOwner: TComponent);
begin
  inherited;
  FThread := TJvThreadTimerClass.Create(True);
  FThread.FreeOnTerminate := True;
  FThread.FDelay := 100;
  FDelay := 100;
  FThread.FOnCall := nil;
  FThread.FSender := Self;
end;

{**************************************************}

destructor TJvThreadTimer.Destroy;
begin
  FThread.Terminate;
  inherited;
end;

{**************************************************}

procedure TJvThreadTimer.SetActive(const Value: Boolean);
begin
  if FActive <> Value then
  begin
    FActive := Value;
    if FActive then
      FThread.Resume
    else
      FThread.Suspend;
  end;
end;

{**************************************************}

procedure TJvThreadTimer.SetDelay(const Value: Integer);
begin
  FDelay := Value;
  FThread.FDelay := FDelay;
end;

{**************************************************}

procedure TJvThreadTimer.SetOnTimer(const Value: TNotifyEvent);
begin
  FOnTimer := Value;
  FThread.FOnCall := FOnTimer;
end;

end.
