object frmSelectColumn: TfrmSelectColumn
  Left = 445
  Top = 244
  Width = 197
  Height = 197
  BorderIcons = []
  BorderStyle = bsSizeToolWin
  BorderWidth = 2
  Caption = 'Select columns'
  Color = clBtnFace
  Constraints.MinHeight = 140
  Constraints.MinWidth = 100
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  FormStyle = fsStayOnTop
  KeyPreview = True
  OldCreateOrder = False
  Scaled = False
  OnActivate = FormActivate
  OnClose = FormClose
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnKeyPress = FormKeyPress
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 0
    Top = 122
    Width = 185
    Height = 41
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 0
    object ButtonOK: TButton
      Left = 56
      Top = 8
      Width = 75
      Height = 25
      Anchors = []
      Caption = '&OK'
      Default = True
      ModalResult = 1
      TabOrder = 0
    end
  end
  object clbList: TCheckListBox
    Left = 0
    Top = 0
    Width = 185
    Height = 122
    OnClickCheck = clbListClickCheck
    Align = alClient
    ItemHeight = 13
    TabOrder = 1
  end
end
