object JvNTEventLogMainForm: TJvNTEventLogMainForm
  Left = 248
  Top = 128
  Width = 613
  Height = 476
  Caption = 'Event Viewer'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Splitter1: TSplitter
    Left = 89
    Top = 38
    Width = 4
    Height = 413
  end
  object ButtonsPanel: TPanel
    Left = 0
    Top = 0
    Width = 605
    Height = 38
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 0
    object btnRefresh: TButton
      Left = 9
      Top = 6
      Width = 75
      Height = 25
      Caption = 'Refresh'
      TabOrder = 0
    end
  end
  object ListBox1: TListBox
    Left = 0
    Top = 38
    Width = 89
    Height = 413
    Align = alLeft
    ItemHeight = 13
    TabOrder = 1
    OnClick = ListBox1Click
  end
  object ListView1: TListView
    Left = 93
    Top = 38
    Width = 512
    Height = 413
    Align = alClient
    Columns = <
      item
        Caption = 'Type'
        Width = 60
      end
      item
        Caption = 'Date'
        Width = 62
      end
      item
        Caption = 'Time'
        Width = 60
      end
      item
        Caption = 'Source'
        Width = 100
      end
      item
        Caption = 'Category'
        Width = 55
      end
      item
        Caption = 'Event'
        Width = 40
      end
      item
        Caption = 'User'
        Width = 70
      end
      item
        Caption = 'Computer'
        Width = 60
      end>
    RowSelect = True
    SortType = stText
    TabOrder = 2
    ViewStyle = vsReport
  end
  object JvNTEventLog1: TJvNTEventLog
    Active = False
    Left = 40
    Top = 48
  end
end
