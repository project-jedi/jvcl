unit RemoteObject;

interface
uses JvUIBSrv, JvUIBObj, RemoteObject_UIB, Classes, JvUIB, JvUIBConst, IdTCPServer;

type
  TRemoteObject = class(TStubRemoteObject)
  private
    FTransaction: TJvUIBTransaction;
  public
    constructor Create(Connection: TIdTCPServerConnection); override;
    destructor Destroy; override;
    { IRemoteInterface }
    function OpenQuery(SQL: String; Stream: TStream): HRESULT; override;
    function GetEmployeeCount(out count: Integer): HRESULT; override;
  end;

implementation

uses main, SysUtils;

{ TRemoteObject }

constructor TRemoteObject.Create(Connection: TIdTCPServerConnection);
begin
  inherited;
  FTransaction := TJvUIBTransaction.Create(nil);
  FTransaction.DataBase := MainForm.DataBase;
end;

destructor TRemoteObject.Destroy;
begin
  FTransaction.Free;
  inherited;
end;

function TRemoteObject.GetEmployeeCount(out count: Integer): HRESULT;
var Query: TJvUIBQuery;
begin
  try
    Query := TJvUIBQuery.Create(FTransaction);
    try
      Query.SQL.Text := 'Select Count(EMP_NO) from employee';
      Query.Open;
      count := Query.Fields.AsInteger[0];
      Result := S_OK;
    finally
      Query.Free;
    end;
  except
    Result := S_FALSE;
  end;
end;

function TRemoteObject.OpenQuery(SQL: String; Stream: TStream): HRESULT;
var Query: TJvUIBQuery;
begin
  try
    Query := TJvUIBQuery.Create(FTransaction);
    try
      Query.FetchBlobs := True;
      Query.SQL.Text := SQL;
      Query.Open;
      Query.FetchAll;
      Query.Fields.SaveToStream(Stream);
      Result := S_OK;
    finally
      Query.Free;
    end;
  except
    Result := S_FALSE;
  end;
end;

initialization
  TJvUIBObjectFactory.Create(JvUIBServer, TRemoteObject, CLSID_RemoteObject);

finalization


end.
