object FormJvclIncConfig: TFormJvclIncConfig
  Left = 140
  Top = 111
  BorderStyle = bsDialog
  Caption = 'JVCL Configuration'
  ClientHeight = 397
  ClientWidth = 744
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  Scaled = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object BevelBorder: TBevel
    Left = 278
    Top = 48
    Width = 462
    Height = 141
  end
  object Label1: TLabel
    Left = 280
    Top = 200
    Width = 457
    Height = 49
    AutoSize = False
    Caption = 
      'jvcl.inc changes are global to all installed Delphi/BCB versions' +
      '.'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clRed
    Font.Height = -13
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
    WordWrap = True
  end
  object CheckListBox: TCheckListBox
    Left = 0
    Top = 48
    Width = 273
    Height = 349
    OnClickCheck = CheckListBoxClickCheck
    Align = alLeft
    ItemHeight = 16
    Style = lbOwnerDrawFixed
    TabOrder = 0
    OnClick = CheckListBoxClick
  end
  object ScrollBox: TScrollBox
    Left = 280
    Top = 49
    Width = 457
    Height = 137
    BorderStyle = bsNone
    TabOrder = 1
    object LblComment: TLabel
      Left = 8
      Top = 8
      Width = 58
      Height = 13
      Caption = 'LblComment'
    end
  end
  object TitlePanel: TPanel
    Left = 0
    Top = 0
    Width = 744
    Height = 44
    Align = alTop
    BevelOuter = bvNone
    Color = clWindow
    TabOrder = 2
    DesignSize = (
      744
      44)
    object PaintBoxWhite: TPaintBox
      Left = 0
      Top = 0
      Width = 744
      Height = 42
      Align = alClient
      OnPaint = PaintBoxWhitePaint
    end
    object imgProjectJEDI: TImage
      Left = 620
      Top = 5
      Width = 116
      Height = 31
      Cursor = crHandPoint
      Hint = 'http://projectjedi.sourceforge.net'
      Anchors = [akTop, akRight]
      AutoSize = True
    end
    object Label4: TLabel
      Left = 8
      Top = 13
      Width = 396
      Height = 18
      Caption = 'Project JEDI   JVCL 3 Installer - jvcl.inc Editor'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -16
      Font.Name = 'Verdana'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object BevelHeader: TBevel
      Left = 0
      Top = 42
      Width = 744
      Height = 2
      Align = alBottom
      Shape = bsBottomLine
    end
  end
  object PanelSpace: TPanel
    Left = 0
    Top = 44
    Width = 744
    Height = 4
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 3
  end
  object BtnCancel: TButton
    Left = 664
    Top = 368
    Width = 75
    Height = 25
    Caption = '&Cancel'
    ModalResult = 2
    TabOrder = 4
  end
  object BtnOk: TButton
    Left = 576
    Top = 368
    Width = 75
    Height = 25
    Caption = '&OK'
    ModalResult = 1
    TabOrder = 5
  end
end
