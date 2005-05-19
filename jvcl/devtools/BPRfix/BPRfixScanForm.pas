unit BPRfixScanForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls, JclFileUtils;

type
  TFormScan = class(TForm)
    Animate1: TAnimate;
    ButtonCancel: TButton;
    LabelDirectory: TLabel;
  private
    { Déclarations privées }
  public
    procedure ChangeDirectory(const FileName: string);
    procedure TaskFinished (const ID: TFileSearchTaskID; const Aborted: Boolean);
  end;

implementation

{$R *.dfm}

{ TFormScan }

procedure TFormScan.ChangeDirectory(const FileName: string);
begin
  LabelDirectory.Caption := FileName;
end;

procedure TFormScan.TaskFinished(const ID: TFileSearchTaskID;
  const Aborted: Boolean);
begin
  ModalResult := mrCancel;
end;

end.

