unit JvFilesU;

interface

uses 
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  FileCtrl, JvDriveCtrls, StdCtrls, JvCombobox, JvListBox, JvLabel,
  JvComponent, JvFileInfo;

type
  TJvFilesFrm = class(TFrame)
    JvLabel6: TJvLabel;
    JvLabel7: TJvLabel;
    JvLabel8: TJvLabel;
    JvLabel9: TJvLabel;
    JvDirectoryListBox1: TJvDirectoryListBox;
    JvDriveCombo1: TJvDriveCombo;
    JvDriveList1: TJvDriveList;
    JvFileListBox1: TJvFileListBox;
    Label1: TLabel;
    Label2: TLabel;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

{$R *.DFM}

end.
