unit fDialogs;
interface
uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, JvCommonExecDlg, JvRenameError, JvDeleteError,
  JvCopyError, JvCommonDialogD, JvDiskPrompt, JvProgressDlg, JvCalculator,
  JvSerialDlg, JvLoginDlg, JvExchListboxes, JvFatalAppExit, JvMessageBeep,
  JvMessageBox, JvPasswordForm, JvDisconnectNetwork, JvConnectNetwork,
  JvAddPrinter, JvOpenAs, JvInputBox, JvSelectDirectory, JvBaseDlg,
  JvShellAbout, JvBrowseFolder, JvFindFiles, JvFormatDrive,
  JvSaveDialog2, JvOpenDialog2, JvOutOfSpaceDlg, JvOutOfMemoryDlg,
  JvObjectPropertiesDlg, JvFindComputerDlg, JvRunDlg, JvChooseIconDlg,
  JvRestartDlg, JvShutdownDlg, JvComponent
  //, JvDialogs
  ;

type
  TForm1 = class(TForm)
    JvFormatDrive1: TJvFormatDrive;
    Button1: TButton;
    Button2: TButton;
    JvFindFiles1: TJvFindFiles;
    JvBrowseFolder1: TJvBrowseFolder;
    Button3: TButton;
    Button4: TButton;
    Button5: TButton;
    Button6: TButton;
    Button7: TButton;
    Button8: TButton;
    JvShellAbout1: TJvShellAbout;
    JvSelectDirectory1: TJvSelectDirectory;
    JvInputBox1: TJvInputBox;
    JvOpenAs1: TJvOpenAs;
    Button9: TButton;
    Button10: TButton;
    Button11: TButton;
    Button12: TButton;
    Button13: TButton;
    Button14: TButton;
    Button15: TButton;
    JvAddPrinter1: TJvAddPrinter;
    JvConnectNetwork1: TJvConnectNetwork;
    JvDisconnectNetwork1: TJvDisconnectNetwork;
    JvPasswordForm1: TJvPasswordForm;
    JvMessageBox1: TJvMessageBox;
    JvMessageBeep1: TJvMessageBeep;
    JvFatalAppExit1: TJvFatalAppExit;
    Button16: TButton;
    Button17: TButton;
    Button18: TButton;
    Button19: TButton;
    Button20: TButton;
    Button21: TButton;
    JvExchListboxes1: TJvExchListboxes;
    JvLoginDlg1: TJvLoginDlg;
    JvSerialDlg1: TJvSerialDlg;
    JvCalculator1: TJvCalculator;
    JvProgressDlg1: TJvProgressDlg;
    JvDiskPrompt1: TJvDiskPrompt;
    JvCopyError1: TJvCopyError;
    Button22: TButton;
    Button23: TButton;
    JvDeleteError1: TJvDeleteError;
    JvRenameError1: TJvRenameError;
    JvShutdownDlg1: TJvShutdownDlg;
    JvRestartDlg1: TJvRestartDlg;
    JvChooseIconDlg1: TJvChooseIconDlg;
    JvRunDlg1: TJvRunDlg;
    JvFindComputerDlg1: TJvFindComputerDlg;
    JvObjectPropertiesDlg1: TJvObjectPropertiesDlg;
    JvOutOfMemoryDlg1: TJvOutOfMemoryDlg;
    JvOutOfSpaceDlg1: TJvOutOfSpaceDlg;
    JvOpenDialog1: TJvOpenDialog2;
    JvSaveDialog1: TJvSaveDialog2;
    Button24: TButton;
    Button25: TButton;
    Button26: TButton;
    Button27: TButton;
    Button28: TButton;
    Button29: TButton;
    Button30: TButton;
    Button31: TButton;
    Button32: TButton;
    Button33: TButton;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure Button6Click(Sender: TObject);
    procedure Button7Click(Sender: TObject);
    procedure Button8Click(Sender: TObject);
    procedure Button9Click(Sender: TObject);
    procedure Button10Click(Sender: TObject);
    procedure Button11Click(Sender: TObject);
    procedure Button12Click(Sender: TObject);
    procedure Button13Click(Sender: TObject);
    procedure Button15Click(Sender: TObject);
    procedure Button14Click(Sender: TObject);
    procedure Button16Click(Sender: TObject);
    procedure Button17Click(Sender: TObject);
    procedure Button18Click(Sender: TObject);
    procedure Button19Click(Sender: TObject);
    procedure Button20Click(Sender: TObject);
    procedure Button21Click(Sender: TObject);
    procedure Button22Click(Sender: TObject);
    procedure Button23Click(Sender: TObject);
    procedure Button24Click(Sender: TObject);
    procedure Button25Click(Sender: TObject);
    procedure Button26Click(Sender: TObject);
    procedure Button27Click(Sender: TObject);
    procedure Button28Click(Sender: TObject);
    procedure Button29Click(Sender: TObject);
    procedure Button30Click(Sender: TObject);
    procedure Button31Click(Sender: TObject);
    procedure Button32Click(Sender: TObject);
    procedure Button33Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;
var
  Form1: TForm1;
implementation
{$R *.DFM}

procedure TForm1.Button1Click(Sender: TObject);
begin
  JvFormatDrive1.FormatDrive('a')
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  JvFindFiles1.Execute;
end;

procedure TForm1.Button3Click(Sender: TObject);
begin
  JvBrowseFolder1.Execute;
end;

procedure TForm1.Button4Click(Sender: TObject);
begin
  JvShellAbout1.ShowModal;
end;

procedure TForm1.Button5Click(Sender: TObject);
begin
  JvSelectDirectory1.Execute;
end;

procedure TForm1.Button6Click(Sender: TObject);
begin
  JvInputBox1.Execute;
end;

procedure TForm1.Button7Click(Sender: TObject);
begin
  JvOpenAs1.Execute;
end;

procedure TForm1.Button8Click(Sender: TObject);
begin
  JvAddPrinter1.Execute;
end;

procedure TForm1.Button9Click(Sender: TObject);
begin
  JvConnectNetwork1.Execute;
end;

procedure TForm1.Button10Click(Sender: TObject);
begin
  JvDisconnectNetwork1.execute;
end;

procedure TForm1.Button11Click(Sender: TObject);
begin
  JvPasswordForm1.Execute;
end;

procedure TForm1.Button12Click(Sender: TObject);
begin
  JvMessageBox1.Execute;
end;

procedure TForm1.Button13Click(Sender: TObject);
begin
  JvMessageBeep1.Execute;
end;

procedure TForm1.Button15Click(Sender: TObject);
begin
  JvFatalAppExit1.Execute;
end;

procedure TForm1.Button14Click(Sender: TObject);
begin
  JvExchListboxes1.Execute;
end;

procedure TForm1.Button16Click(Sender: TObject);
begin
  JvLoginDlg1.Execute;
end;

procedure TForm1.Button17Click(Sender: TObject);
begin
  JvSerialDlg1.Execute;
end;

procedure TForm1.Button18Click(Sender: TObject);
begin
  JvCalculator1.Execute;
end;

procedure TForm1.Button19Click(Sender: TObject);
var
 i:integer;
begin
  JvProgressDlg1.Maximum:=100;
  JvProgressDlg1.Show;
  for i:=1 to 100 do
  begin
    JvProgressDlg1.Value:=i;
    sleep(20);
  end;
  JvProgressDlg1.Close;
end;

procedure TForm1.Button20Click(Sender: TObject);
begin
  JvDiskPrompt1.Execute;
end;

procedure TForm1.Button21Click(Sender: TObject);
begin
  JvCopyError1.Execute;
end;

procedure TForm1.Button22Click(Sender: TObject);
begin
  JvDeleteError1.Execute;
end;

procedure TForm1.Button23Click(Sender: TObject);
begin
  JvRenameError1.Execute;
end;

procedure TForm1.Button24Click(Sender: TObject);
begin
  JvShutdownDlg1.Execute;
end;

procedure TForm1.Button25Click(Sender: TObject);
begin
  JvRestartDlg1.Execute;
end;

procedure TForm1.Button26Click(Sender: TObject);
begin
  JvChooseIconDlg1.Execute;
end;

procedure TForm1.Button27Click(Sender: TObject);
begin
  JvRunDlg1.Execute;
end;

procedure TForm1.Button28Click(Sender: TObject);
begin
  JvFindComputerDlg1.Execute;
end;

procedure TForm1.Button29Click(Sender: TObject);
begin
  JvObjectPropertiesDlg1.Execute;
end;

procedure TForm1.Button30Click(Sender: TObject);
begin
  JvOutOfMemoryDlg1.Execute;
end;

procedure TForm1.Button31Click(Sender: TObject);
begin
  JvOutOfSpaceDlg1.Execute;
end;

procedure TForm1.Button32Click(Sender: TObject);
begin
  JvOpenDialog1.Execute;
end;

procedure TForm1.Button33Click(Sender: TObject);
begin
  JvSaveDialog1.Execute;
end;

end.
