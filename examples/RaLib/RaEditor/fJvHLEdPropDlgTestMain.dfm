object Form1: TForm1
  Left = 357
  Top = 162
  Width = 410
  Height = 295
  Caption = 'Form1'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = True
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object RAHLEditor1: TJvHLEditor
    Left = 16
    Top = 16
    Width = 249
    Height = 233
    Cursor = crIBeam
    Lines.Strings = (
      'unit fMain;'
      ''
      'interface'
      ''
      'uses'
      
        '  Windows, Messages, SysUtils, Classes, Graphics, Controls, Form' +
        's, Dialogs,'
      
        '  JvHLEdPropDlg, StdCtrls, Buttons, JvButtons, JvEditor, RAHLEdi' +
        'tor;'
      ''
      'type'
      '  TForm1 = class(TForm)'
      '    RAHLEditor1: TJvHLEditor;'
      '    RAhtButton1: TJvHTButton;'
      '    RAHLEdPropDlg1: TJvHLEdPropDlg;'
      '    procedure RAhtButton1Click(Sender: TObject);'
      '  private'
      '    { Private declarations }'
      '  public'
      '    { Public declarations }'
      '  end;'
      ''
      'var'
      '  Form1: TForm1;'
      ''
      'implementation'
      ''
      '{$R *.DFM}'
      ''
      'procedure TForm1.RAhtButton1Click(Sender: TObject);'
      'begin'
      '  RAHLEdPropDlg1.Execute;'
      'end;'
      ''
      'end.')
    GutterWidth = 0
    RightMarginColor = clSilver
    Completion.ItemHeight = 13
    Completion.Interval = 800
    Completion.ListBoxStyle = lbStandard
    Completion.CaretChar = '|'
    Completion.CRLF = '/n'
    Completion.Separator = '='
    TabStops = '3 5'
    SelForeColor = clHighlightText
    SelBackColor = clHighlight
    Ctl3D = True
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Courier New'
    Font.Style = []
    ParentColor = False
    ParentFont = False
    TabStop = True
    UseDockManager = False
    Colors.Comment.Style = [fsItalic]
    Colors.Comment.ForeColor = clOlive
    Colors.Number.ForeColor = clNavy
    Colors.Strings.ForeColor = clMaroon
    Colors.Symbol.ForeColor = clBlue
    Colors.Reserved.Style = [fsBold]
    Colors.Preproc.ForeColor = clGreen
    Colors.Statement.Style = [fsBold]
    DelphiColors = False
  end
  object RAhtButton1: TJvHTButton
    Left = 280
    Top = 16
    Width = 105
    Height = 25
    Caption = 'Properties'
    TabOrder = 1
    OnClick = RAhtButton1Click
  end
  object RegAuto1: TJvFormStorage
    StoredValues = <>
    Left = 128
    Top = 136
  end
  object JvHLEdPropDlg1: TJvHLEdPropDlg
    JvHLEditor = RAHLEditor1
    RegAuto = RegAuto1
    Pages = [epEditor, epColors]
    OnDialogPopup = RAHLEdPropDlg1DialogPopup
    OnDialogClosed = RAHLEdPropDlg1DialogClosed
    Left = 312
    Top = 104
  end
end
