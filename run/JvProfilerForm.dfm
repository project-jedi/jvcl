object ProfReport: TProfReport
  Left = 200
  Top = 114
  ActiveControl = lvReport
  BorderIcons = [biSystemMenu]
  Caption = 'Profiler Report'
  ClientHeight = 264
  ClientWidth = 445
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clBlack
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  Icon.Data = {
    0000010001002020040000000000E80200001600000028000000200000004000
    0000010004000000000000020000000000000000000000000000000000000000
    000000008000008000000080800080000000800080008080000080808000C0C0
    C0000000FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF000000
    0777777777777777777777777700000077777777777777777777777777700030
    000000000000000000000007777703BBBBBBBBBBBBBBBBBBBBBBBB8077773BBB
    BBBBBBBBBBBBBBBBBBBBBBB807773BBBBBBBBBBBBBBBBBBBBBBBBBBB07773BBB
    BBBBBBBBB8008BBBBBBBBBBB07703BBBBBBBBBBBB0000BBBBBBBBBB8077003BB
    BBBBBBBBB0000BBBBBBBBBB0770003BBBBBBBBBBB8008BBBBBBBBB807700003B
    BBBBBBBBBBBBBBBBBBBBBB077000003BBBBBBBBBBB0BBBBBBBBBB80770000003
    BBBBBBBBB808BBBBBBBBB07700000003BBBBBBBBB303BBBBBBBB807700000000
    3BBBBBBBB000BBBBBBBB0770000000003BBBBBBB80008BBBBBB8077000000000
    03BBBBBB30003BBBBBB077000000000003BBBBBB00000BBBBB80770000000000
    003BBBBB00000BBBBB07700000000000003BBBBB00000BBBB807700000000000
    0003BBBB00000BBBB0770000000000000003BBBB00000BBB8077000000000000
    00003BBB80008BBB077000000000000000003BBBBBBBBBB80770000000000000
    000003BBBBBBBBB07700000000000000000003BBBBBBBB807700000000000000
    0000003BBBBBBB0770000000000000000000003BBBBBB8077000000000000000
    00000003BBBBB077000000000000000000000003BBBB80700000000000000000
    000000003BB8000000000000000000000000000003330000000000000000F800
    0003F0000001C000000080000000000000000000000000000001000000018000
    000380000003C0000007C0000007E000000FE000000FF000001FF000001FF800
    003FF800003FFC00007FFC00007FFE0000FFFE0000FFFF0001FFFF0001FFFF80
    03FFFF8003FFFFC007FFFFC007FFFFE00FFFFFE01FFFFFF07FFFFFF8FFFF}
  OldCreateOrder = True
  Position = poScreenCenter
  ShowHint = True
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 0
    Top = 231
    Width = 445
    Height = 33
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 0
    ExplicitTop = 233
    DesignSize = (
      445
      33)
    object SaveBtn: TButton
      Left = 8
      Top = 5
      Width = 75
      Height = 25
      Hint = 'Save report to a file (compatible with Excel)'
      Anchors = [akLeft, akBottom]
      Caption = '&Save...'
      TabOrder = 0
      OnClick = SaveBtnClick
    end
    object Panel2: TPanel
      Left = 270
      Top = 0
      Width = 175
      Height = 33
      Align = alRight
      BevelOuter = bvNone
      TabOrder = 1
      DesignSize = (
        175
        33)
      object OKBtn: TButton
        Left = 60
        Top = 6
        Width = 75
        Height = 25
        Hint = 'Close report window'
        Anchors = [akRight, akBottom]
        Cancel = True
        Caption = '&Close'
        Default = True
        ModalResult = 1
        TabOrder = 0
        OnClick = OKBtnClick
      end
    end
    object TrimBtn: TButton
      Left = 96
      Top = 5
      Width = 75
      Height = 25
      Hint = 'Remove unused calls from the list'
      Anchors = [akLeft, akBottom]
      Caption = '&Trim'
      TabOrder = 2
      OnClick = TrimBtnClick
    end
  end
  object lvReport: TListView
    Left = 0
    Top = 0
    Width = 445
    Height = 231
    Hint = 'Click the top column to sort the items'
    Align = alClient
    BorderStyle = bsNone
    Columns = <
      item
        Caption = 'Function / Procedure '
        Width = 160
      end
      item
        Alignment = taRightJustify
        Caption = 'Total time (ms)'
        Width = 80
      end
      item
        Alignment = taRightJustify
        Caption = 'Calls'
        Width = 35
      end
      item
        Alignment = taRightJustify
        Caption = 'Average time (ms)'
        Width = 100
      end
      item
        Alignment = taRightJustify
        Caption = 'Percent (%)'
        Width = 70
      end>
    GridLines = True
    MultiSelect = True
    RowSelect = True
    TabOrder = 1
    ViewStyle = vsReport
    OnColumnClick = lvReportColumnClick
    ExplicitHeight = 233
  end
end
