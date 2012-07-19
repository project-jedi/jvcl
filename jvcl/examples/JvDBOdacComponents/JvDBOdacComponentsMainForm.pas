unit JvDBOdacComponentsMainForm;

{$I jvcl.inc}

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, DB, DBAccess, Ora, StdCtrls, Grids, DBGrids, ExtCtrls,
  JvDBLogonDialogOdac, JvBaseDlg, JvBaseDBDialog, JvBaseDBPasswordDialog,
  JvDBPasswordDialogOdac, MemDS, OraSmart, JvOdacSmartQuery, JvDBActions,
  ActnList, JvActionsEngine, Buttons, JvDBLogonDialogBaseDevart,
  JvDBActionsEngineDatasetDevart,
  {$IFDEF USE_3RDPARTY_DEVEXPRESS_CXEDITOR}
  JvDynControlEngineDevExpCx,
  {$ELSE}
  JvDynControlEngineJVCL,
  {$ENDIF}
  JvComponentBase, JvAppStorage, JvAppRegistryStorage;

type
  TForm1 = class(TForm)
    OraSession1: TOraSession;
    JvOdacSmartQuery1: TJvOdacSmartQuery;
    JvDBOdacPasswordDialog1: TJvDBOdacPasswordDialog;
    JvDBOdacConnectDialog1: TJvDBOdacConnectDialog;
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
    SQLMemo: TMemo;
    JvAppRegistryStorage1: TJvAppRegistryStorage;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.Button1Click(Sender: TObject);
begin
//  OraSession1.Connect;
  JvDBOdacConnectDialog1.Execute;
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  OraSession1.Disconnect;
end;

procedure TForm1.Button3Click(Sender: TObject);
begin
  JvOdacSmartQuery1.SQL.Text := SQLMemo.Text;
  JvOdacSmartQuery1.Open;
end;

procedure TForm1.Button4Click(Sender: TObject);
begin
  JvOdacSmartQuery1.Close;
end;

procedure TForm1.Button5Click(Sender: TObject);
begin
  JvDBOdacPasswordDialog1.Execute;
end;

end.
