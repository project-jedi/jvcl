unit CaptionEditForm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls;

type
  TCaptionEditDlg = class(TForm)
    Label1: TLabel;
    OkBtn: TButton;
    CancelBtn: TButton;
    CaptionEdit: TMemo;
    FontBtn: TButton;
    FontDialog1: TFontDialog;
    procedure FontBtnClick(Sender: TObject);
  private
  public
    class procedure NewCaption(var TheCaption : string;TheFont : TFont);
  end;


implementation

{$R *.DFM}

class procedure TCaptionEditDlg.NewCaption(var TheCaption : string;TheFont : TFont);
begin {NewCaption}
  with TCaptionEditDlg.Create(Application) do begin
    try
      CaptionEdit.Text := TheCaption;
      FontDialog1.Font := TheFont;

      if ShowModal = mrOk then begin
        TheCaption := CaptionEdit.Text;
        TheFont.Assign(CaptionEdit.Font);
      end;
    finally
      Release;
    end;
  end;
end;  {NewCaption}


procedure TCaptionEditDlg.FontBtnClick(Sender: TObject);
begin
  if FontDialog1.Execute then begin
    CaptionEdit.Font := FontDialog1.Font;
  end;
end;


end.
