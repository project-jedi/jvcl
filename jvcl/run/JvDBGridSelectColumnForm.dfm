object frmSelectColumn: TfrmSelectColumn
  Left = 445
  Top = 244
  Width = 209
  Height = 142
  BorderIcons = [biSystemMenu]
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
  KeyPreview = True
  OldCreateOrder = False
  OnActivate = FormActivate
  OnClose = FormClose
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnKeyPress = FormKeyPress
  OnResize = FormResize
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 0
    Top = 50
    Width = 189
    Height = 50
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 0
    DesignSize = (
      189
      50)
    object cbWithFieldName: TCheckBox
      Left = 18
      Top = -2
      Width = 165
      Height = 25
      Anchors = [akLeft, akRight]
      TabOrder = 0
      Visible = False
      OnClick = cbClick
    end
    object ButtonOK: TButton
      Left = 56
      Top = 19
      Width = 75
      Height = 25
      Anchors = []
      Caption = '&OK'
      Default = True
      ModalResult = 1
      TabOrder = 1
    end
  end
  object clbList: TCheckListBox
    Left = 0
    Top = 0
    Width = 189
    Height = 50
    OnClickCheck = clbListClickCheck
    Align = alClient
    DragMode = dmAutomatic
    ItemHeight = 13
    TabOrder = 1
    OnClick = clbListClick
    OnDragOver = clbListDragOver
    OnKeyDown = clbListKeyDown
    OnMouseMove = clbListMouseMove
    OnStartDrag = clbListStartDrag
  end
end
