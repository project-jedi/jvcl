unit JvDBUniDacComponentsMainForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, DB, DBAccess, StdCtrls, Grids, DBGrids, ExtCtrls,
  JvDBLogonDialogOdac, JvBaseDlg, JvBaseDBDialog, JvBaseDBPasswordDialog,
  JvDBPasswordDialogOdac, MemDS, JvDBActions,
  ActnList, JvActionsEngine, Buttons,
  JvDBActionsEngineDatasetDevart,
  Uni, UniDacVcl, JvUniDacQuery,
  JvDBLogonDialogUniDac, SQLiteUniProvider, SQLServerUniProvider,
  PostgreSQLUniProvider, OracleUniProvider, MySQLUniProvider, InterBaseUniProvider, DBFUniProvider,
  DB2UniProvider, ASEUniProvider, AdvantageUniProvider, UniProvider, ODBCUniProvider, AccessUniProvider, JvComponentBase,
  JvAppStorage, JvAppRegistryStorage, JvDBLogonDialogBaseDevart;

type
  TForm1 = class(TForm)
    Panel1: TPanel;
    DBGrid1: TDBGrid;
    DataSource1: TDataSource;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    Button5: TButton;
    Panel2: TPanel;
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    BitBtn3: TBitBtn;
    BitBtn4: TBitBtn;
    BitBtn5: TBitBtn;
    BitBtn6: TBitBtn;
    BitBtn7: TBitBtn;
    BitBtn8: TBitBtn;
    BitBtn9: TBitBtn;
    BitBtn10: TBitBtn;
    BitBtn11: TBitBtn;
    BitBtn12: TBitBtn;
    BitBtn13: TBitBtn;
    JvDatabaseActionList1: TJvDatabaseActionList;
    JvDatabaseSingleRecordWindowAction1: TJvDatabaseSingleRecordWindowAction;
    JvDatabaseFirstAction1: TJvDatabaseFirstAction;
    JvDatabaseLastAction1: TJvDatabaseLastAction;
    JvDatabaseNextAction1: TJvDatabaseNextAction;
    JvDatabasePriorAction1: TJvDatabasePriorAction;
    JvDatabaseNextBlockAction1: TJvDatabaseNextBlockAction;
    JvDatabasePriorBlockAction1: TJvDatabasePriorBlockAction;
    JvDatabasePositionAction1: TJvDatabasePositionAction;
    JvDatabaseRefreshAction1: TJvDatabaseRefreshAction;
    JvDatabaseInsertAction1: TJvDatabaseInsertAction;
    JvDatabaseCopyAction1: TJvDatabaseCopyAction;
    JvDatabaseEditAction1: TJvDatabaseEditAction;
    JvDatabaseDeleteAction1: TJvDatabaseDeleteAction;
    JvDatabasePostAction1: TJvDatabasePostAction;
    JvDatabaseCancelAction1: TJvDatabaseCancelAction;
    JvDatabaseSimpleAction1: TJvDatabaseSimpleAction;
    JvDatabaseOpenAction1: TJvDatabaseOpenAction;
    JvDatabaseCloseAction1: TJvDatabaseCloseAction;
    JvDatabaseModifyAllAction1: TJvDatabaseModifyAllAction;
    JvDatabaseShowSQLStatementAction1: TJvDatabaseShowSQLStatementAction;
    JvDatabaseSimpleAction2: TJvDatabaseSimpleAction;
    JvDatabaseEditAction2: TJvDatabaseEditAction;
    UniConnection1: TUniConnection;
    AccessUniProvider1: TAccessUniProvider;
    AdvantageUniProvider1: TAdvantageUniProvider;
    ASEUniProvider1: TASEUniProvider;
    DB2UniProvider1: TDB2UniProvider;
    DBFUniProvider1: TDBFUniProvider;
    InterBaseUniProvider1: TInterBaseUniProvider;
    MySQLUniProvider1: TMySQLUniProvider;
    ODBCUniProvider1: TODBCUniProvider;
    OracleUniProvider1: TOracleUniProvider;
    PostgreSQLUniProvider1: TPostgreSQLUniProvider;
    SQLServerUniProvider1: TSQLServerUniProvider;
    SQLiteUniProvider1: TSQLiteUniProvider;
    Panel3: TPanel;
    SQLMemo: TMemo;
    JvUniDacUniQuery1: TJvUniDacUniQuery;
    JvDBUniDacConnectDialog1: TJvDBUniDacConnectDialog;
    JvAppRegistryStorage1: TJvAppRegistryStorage;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
  private
    FJvDBUniDacConnectDialog: TJvDBUniDacConnectDialog;
    { Private declarations }
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure GetSQLStatement;
    property JvDBUniDacConnectDialog: TJvDBUniDacConnectDialog read FJvDBUniDacConnectDialog;
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

constructor TForm1.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FJvDBUniDacConnectDialog := TJvDBUniDacConnectDialog.Create(self);
end;

destructor TForm1.Destroy;
begin
  FreeAndNil(FJvDBUniDacConnectDialog);
  inherited Destroy;
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  UniConnection1.Connect;
  GetSQLStatement;
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  UniConnection1.Disconnect;
end;

procedure TForm1.Button3Click(Sender: TObject);
begin
  JvUniDacUniQuery1.SQL.Assign(SQLMemo.Lines);
  JvUniDacUniQuery1.Open;
end;

procedure TForm1.Button4Click(Sender: TObject);
begin
  JvUniDacUniQuery1.Close;
end;

procedure TForm1.GetSQLStatement;
var
  TableNames: TStringList;
  i: Integer;
begin
  TableNames := TStringList.Create;
  try
    UniConnection1.Open;
    UniConnection1.GetTableNames(TableNames, False);
    for i := 0 to TableNames.Count - 1 do
    begin
      SQLMemo.Lines.Text := 'SELECT * FROM '+TableNames[i];
      Break;
    end;
  finally
    TableNames.Free;
  end;
end;

end.
