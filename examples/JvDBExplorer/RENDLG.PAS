{*******************************************************}
{                                                       }
{     Delphi VCL Extensions (RX) demo program           }
{                                                       }
{     Copyright (c) 1996 AO ROSNO                       }
{                                                       }
{*******************************************************}

unit RenDlg;

interface

uses Windows, SysUtils, Classes, Graphics, Forms, Controls, StdCtrls,
  Buttons, ExtCtrls, JvPlacemnt, Mask, JvToolEdit;

type
  TRenameDialog = class(TForm)
    OKBtn: TButton;
    CancelBtn: TButton;
    TabnameEdit: TJvFilenameEdit ;
    Label1: TLabel;
    TabnameView: TEdit;
    Label2: TLabel;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

function RenameTableDialog(const CurrName, Directory: string;
  var NewName: string): Boolean;

implementation

{$R *.DFM}

function RenameTableDialog(const CurrName, Directory: string;
  var NewName: string): Boolean;
begin
  Result := False;
  NewName := CurrName;
  with TRenameDialog.Create(Application) do
  try
    TabnameView.Text := CurrName;
    TabnameEdit.Text := CurrName;
    TabnameEdit.InitialDir := Directory;
    if (ShowModal = mrOk) then begin
      NewName := TabnameEdit.Text;
      Result := (CurrName <> NewName);
    end;
  finally
    Free;
  end;
end;

end.
