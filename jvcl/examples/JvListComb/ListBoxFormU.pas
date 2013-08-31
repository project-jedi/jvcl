unit ListBoxFormU;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, JvExStdCtrls, JvListBox,
  Vcl.ExtCtrls, JvCombobox, JvColorCombo, Vcl.ComCtrls;

type
  TfmListBox = class(TForm)
    pnl1: TPanel;
    lst1: TJvListBox;
    btnTextGen: TButton;
    chkWW: TCheckBox;
    cbcMain: TJvColorComboBox;
    cbcAlt: TJvColorComboBox;
    chkAlt: TCheckBox;
    chkOutline: TCheckBox;
    pgcLBs: TPageControl;
    tsJVCL: TTabSheet;
    tsVCL: TTabSheet;
    lst2: TListBox;
    procedure btnTextGenClick(Sender: TObject);
    procedure chkWWClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure chkAltClick(Sender: TObject);
    procedure cbcAltChange(Sender: TObject);
    procedure cbcMainChange(Sender: TObject);
    procedure chkOutlineClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  fmListBox: TfmListBox;

implementation

{$R *.dfm}

procedure TfmListBox.btnTextGenClick(Sender: TObject);
var ln_l, wd_l, L_cnt: integer; c: char;
    sb: TStringBuilder; sl: TStrings;
const lines = 11;
begin
  sl := TStringList.Create;
  try
    sb := TStringBuilder.Create;
    try
      for L_cnt := 1 to lines do begin
          sb.Clear; // new line
          ln_l := random(100) + 30; // line length
          while sb.Length < ln_l do begin
            wd_l := random(12) + 4; // word length
            c := Char(random(ord('Z')-ord('A')+1) + ord('a') );

            sb.Append(StringOfChar(c, wd_l));
            sb.Append(' ');
          end;
          sl.Add(Trim(sb.ToString));
      end;
    finally
      sb.Free;
    end;

    lst1.Items := sl;
    lst2.Items := sl;
  finally
    sl.Free;
  end;
end;

procedure TfmListBox.chkWWClick(Sender: TObject);
begin
  lst1.MultiLine := chkWW.Checked;
end;

procedure TfmListBox.FormCreate(Sender: TObject);
begin
  lst1.ColorAlternate := clLime;
  lst1.Color := clYellow;
end;

procedure TfmListBox.FormShow(Sender: TObject);
begin
  chkWW.Checked := lst1.MultiLine;

  cbcMain.ColorValue := lst1.Color;
  cbcAlt.ColorValue  := lst1.ColorAlternate;

  chkAlt.Checked := lst1.ColorAlternate <> lst1.Color;
  cbcAlt.Enabled := chkAlt.Checked;
end;

procedure TfmListBox.chkAltClick(Sender: TObject);
begin
  if chkAlt.Checked
     then lst1.ColorAlternate := cbcAlt.ColorValue
     else lst1.ColorAlternate := lst1.Color;

  cbcAlt.Enabled := chkAlt.Checked;
end;

procedure TfmListBox.chkOutlineClick(Sender: TObject);
begin
  lst1.SeparateItems := chkOutline.Checked;
end;

procedure TfmListBox.cbcAltChange(Sender: TObject);
begin
  lst1.ColorAlternate := cbcAlt.ColorValue
end;

procedure TfmListBox.cbcMainChange(Sender: TObject);
begin
  lst1.Color := cbcMain.ColorValue
end;

end.
