//-----------------------------------------------------------------------
// A fast timer.  Based upon original code (TmwFastTime) by Martin
// Waldenburg in 1996.  This version descends from TObject, so we
// don't have to mess around installing it on the component palette.
//-----------------------------------------------------------------------

unit FastTime;

interface

uses
  SysUtils, Windows, Classes;

type
    TFastTimer = class(TObject)
    private
        nStart, nStop: TLargeInteger;
        function GetElapsedTime: String;
        function GetElapsed: Extended;
        function GetElapsedMicroSeconds: TLargeInteger;
    public
        property Elapsed: Extended read GetElapsed;
        property ElapsedMicroseconds: TLargeInteger read GetElapsedMicroSeconds;
        property ElapsedTime: String read GetElapsedTime;
        procedure Start;
        procedure Stop;
    end;

var
    FastTimer: TFastTimer;
    Frequency: TLargeInteger;

implementation

function TFastTimer.GetElapsed: Extended;
begin
    Result:= (nStop - nStart) / Frequency;
end;

function TFastTimer.GetElapsedTime: String;
begin
    Result := Format ('Seconds: %g', [GetElapsed]);
end;

function TFastTimer.GetElapsedMicroSeconds: TLargeInteger;
begin
    Result := Trunc (GetElapsed * 1000000.0);
end;

procedure TFastTimer.Start;
begin
    QueryPerformanceCounter (nStart);
end;

procedure TFastTimer.Stop;
begin
    QueryPerformanceCounter (nStop);
end;

initialization
    QueryPerformanceFrequency (Frequency);
    FastTimer := TFastTimer.Create;
finalization
    FastTimer.Free;
end.

