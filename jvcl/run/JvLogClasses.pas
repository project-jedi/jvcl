unit JvLogClasses;

{$I jvcl.inc}

interface

uses
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  SysUtils, Contnrs,
  JclBase;

type
  TJvLogEventSeverity = (lesError, lesWarning, lesInformation);

  TJvLogRecord = class(TObject)
  public
    Time: string;
    Title: string;
    Description: string;
    Severity : TJvLogEventSeverity;

    function GetOutputString: string;
  end;

  TJvLogRecordList = class(TObjectList)
  private
    function GetItem(Index: TJclListSize): TJvLogRecord;
    procedure SetItem(Index: TJclListSize; const ALogRecord: TJvLogRecord);
  public
    property Items[Index: TJclListSize]: TJvLogRecord read GetItem write SetItem; default;
  end;

{$IFDEF UNITVERSIONING}
const
  UnitVersioning: TUnitVersionInfo = (
    RCSfile: '$URL$';
    Revision: '$Revision$';
    Date: '$Date$';
    LogPath: 'JVCL\run'
  );
{$ENDIF UNITVERSIONING}

function GetSeverityString( const Severity : TJvLogEventSeverity) : string;
function GetSeverityFromString( const SeverityString : string) : TJvLogEventSeverity;


implementation

resourcestring
  STR_SEVERITY_INFORMATION = 'Information';
  STR_SEVERITY_WARNING     = 'Warning';
  STR_SEVERITY_ERROR       = 'Error';

function GetSeverityString( const Severity : TJvLogEventSeverity) : string;
begin
  case Severity of
    lesError:
      result := STR_SEVERITY_ERROR;
    lesWarning:
      result := STR_SEVERITY_WARNING;
    lesInformation:
      result := STR_SEVERITY_INFORMATION;
  end;
end;

function GetSeverityFromString( const SeverityString : string) : TJvLogEventSeverity;
begin
  if SeverityString = STR_SEVERITY_ERROR then
    Result := lesError
  else if SeverityString = STR_SEVERITY_WARNING then
    Result := lesWarning
  else
    Result := lesInformation;
end;

// === { TJvLogRecord } =======================================
function TJvLogRecord.GetOutputString: string;
begin
  Result := '[' + Time + ']' + GetSeverityString( Severity) + '>' +
            StringReplace(Title, '>', '>>', [rfReplaceAll]) +
            '>' + Description + sLineBreak;
end;

// === { TJvLogRecordList } ===================================

function TJvLogRecordList.GetItem(Index: TJclListSize): TJvLogRecord;
begin
  Result := TJvLogRecord(inherited Items[Index]);
end;

procedure TJvLogRecordList.SetItem(Index: TJclListSize;
  const ALogRecord: TJvLogRecord);
begin
  inherited Items[Index] := ALogRecord;
end;

end.
