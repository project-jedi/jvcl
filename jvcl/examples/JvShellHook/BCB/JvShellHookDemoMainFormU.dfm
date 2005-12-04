object JvShellHookDemoMainForm: TJvShellHookDemoMainForm
  Left = 229
  Top = 147
  Width = 545
  Height = 282
  Caption = 'JvShellHook Demo'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  DesignSize = (
    537
    255)
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 8
    Width = 59
    Height = 13
    Caption = '&Messages:'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Shell Dlg 2'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object btnClear: TButton
    Left = 448
    Top = 209
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = '&Clear'
    TabOrder = 0
    OnClick = btnClearClick
  end
  object chkActive: TCheckBox
    Left = 16
    Top = 199
    Width = 97
    Height = 17
    Anchors = [akLeft, akBottom]
    Caption = '&Active'
    TabOrder = 1
    OnClick = chkActiveClick
  end
  object chkNoRedraw: TCheckBox
    Left = 16
    Top = 222
    Width = 121
    Height = 17
    Anchors = [akLeft, akBottom]
    Caption = 'Don'#39't show &redraws'
    Checked = True
    State = cbChecked
    TabOrder = 2
  end
  object lvMessages: TListView
    Left = 8
    Top = 32
    Width = 515
    Height = 157
    Anchors = [akLeft, akTop, akRight, akBottom]
    BorderStyle = bsNone
    Columns = <
      item
        Caption = 'Message'
        Width = 200
      end
      item
        Caption = 'WParam'
        Width = 100
      end
      item
        Caption = 'LParam'
        Width = 100
      end
      item
        Caption = 'Result'
        Width = 115
      end>
    ColumnClick = False
    HideSelection = False
    ReadOnly = True
    RowSelect = True
    TabOrder = 3
    ViewStyle = vsReport
    OnResize = lvMessagesResize
  end
end
