{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvAppearingLabel.PAS, released on 2001-02-28.

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

unit JvAppearingLabel;

{$OBJEXPORTALL On}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  JvLabel, JvTypes;

type
  TJvAppearThread = class(TThread)
  protected
    procedure Draw;
    procedure Execute; override;
  public
    FDelay: Cardinal;
    FOnDraw: TNotifyEvent;
  end;

  TJvAppearingLabel = class(TJvLabel)
  private
    FAutoStart: Boolean;
    FInterval: Cardinal;
    FPixel: Integer;
    FAppear: TDirection;
    FOnAppeared: TNotifyEvent;
    FForm: TControl;
    FDestWidth: Integer;
    FDestHeight: Integer;
    FDestLeft: Integer;
    FDestTop: Integer;
    FFirst: Cardinal;
    FThread: TJvAppearThread;
    FTag: Integer;
    procedure SetAutoStart(const Value: Boolean);
    procedure SetInterval(const Value: Cardinal);
    procedure Appearing(Sender: TObject);
    procedure SetFirst(const Value: Cardinal);
  protected
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property FirstInterval: Cardinal read FFirst write SetFirst default 100;
    property Interval: Cardinal read FInterval write SetInterval default 20;
    property ScrollPixel: Integer read FPixel write FPixel default 3;
    property AutoStart: Boolean read FAutoStart write SetAutoStart default False;
    property AppearFrom: TDirection read FAppear write FAppear default drFromRight;
    property OnAppeared: TNotifyEvent read FOnAppeared write FOnAppeared;
    procedure Appear;
  end;

implementation

///////////////////////////////////////////////////////////
// TJvAppearingLabel
///////////////////////////////////////////////////////////

procedure TJvAppearingLabel.Appear;
begin
  FTag := 0;
  FThread.Resume;
end;

{**************************************************}

procedure TJvAppearingLabel.Appearing(Sender: TObject);
begin
  if FTag = 0 then
  begin
    FDestLeft := Left;
    FDestTop := Top;
    FDestWidth := Width;
    FDestHeight := Height;
    case FAppear of
      drFromRight:
        begin
          Left := FForm.ClientWidth;
          Width := 0;
        end;
      drFromLeft:
        Left := -Width;
      drFromTop:
        Top := -Height;
      drFromBottom:
        begin
          Top := FForm.ClientHeight;
          Height := 0;
        end;
    end;
    FTag := 1;
    FThread.FDelay := FInterval;
    Visible := True;
  end;
  case FAppear of
    drFromRight:
      begin
        if Abs(Left - FDestLeft) < FPixel then
        begin
          Left := FDestLeft;
          FThread.Suspend;
          if Assigned(FOnAppeared) then
            FOnAppeared(Self);
        end
        else
          Left := Left - FPixel;
        if Width <> FDestWidth then
        begin
          if Left + FDestWidth < FForm.ClientWidth then
            Width := FDestWidth
          else
            Width := FForm.ClientWidth - Left - 2;
        end;
      end;
    drFromLeft:
      begin
        if Abs(Left - FDestLeft) < FPixel then
        begin
          Left := FDestLeft;
          FThread.Suspend;
          if Assigned(FOnAppeared) then
            FOnAppeared(Self);
        end
        else
          Left := Left + FPixel;
      end;
    drFromTop:
      begin
        if Abs(Top - FDestTop) < FPixel then
        begin
          Top := FDestTop;
          FThread.Suspend;
          if Assigned(FOnAppeared) then
            FOnAppeared(Self);
        end
        else
          Top := Top + FPixel;
      end;
    drFromBottom:
      begin
        if Abs(Top - FDestTop) < FPixel then
        begin
          Top := FDestTop;
          FThread.Suspend;
          if Assigned(FOnAppeared) then
            FOnAppeared(Self);
        end
        else
          Top := Top - FPixel;
        if Height <> FDestHeight then
        begin
          if Top + FDestHeight < FForm.ClientHeight then
            Height := FDestHeight
          else
            Height := FForm.ClientHeight - Top - 2;
        end;
      end;
  end;
end;

{**************************************************}

constructor TJvAppearingLabel.Create(AOwner: TComponent);
begin
  inherited;
  FFirst := 100;
  FInterval := 20;
  FPixel := 3;
  FAutoStart := False;
  FAppear := drFromRight;
  FForm := AOwner as TControl;
  FTag := 0;

  FThread := TJvAppearThread.Create(True);
  FThread.FreeOnTerminate := False;
  FThread.FDelay := FInterval;
  FThread.FOnDraw := Appearing;
end;

{**************************************************}

destructor TJvAppearingLabel.Destroy;
begin
  FThread.Terminate;
  while (not FThread.Terminated) do
    Application.ProcessMessages;
  FThread.Free;
  inherited;
end;

{**************************************************}

procedure TJvAppearingLabel.SetAutoStart(const Value: Boolean);
begin
  FAutoStart := Value;
  if Value then
    Visible := False;
  if Value and not (csDesigning in ComponentState) then
  begin
    if Value then
      FThread.Resume
    else
      FThread.Suspend;
  end;
end;

{**************************************************}

procedure TJvAppearingLabel.SetFirst(const Value: Cardinal);
begin
  FFirst := Value;
  if FTag = 0 then
    FThread.FDelay := FFirst;
end;

{**************************************************}

procedure TJvAppearingLabel.SetInterval(const Value: Cardinal);
begin
  FInterval := Value;
  if FTag <> 0 then
    FThread.FDelay := Value;
end;

///////////////////////////////////////////////////////////
// TJvAppearThread
///////////////////////////////////////////////////////////

procedure TJvAppearThread.Draw;
begin
  if Assigned(FOnDraw) then
    FOnDraw(nil);
end;
{*******************************************************}

procedure TJvAppearThread.Execute;
begin
  while not Terminated do
  begin
    Synchronize(Draw);
    Sleep(FDelay);
  end;
end;

end.
