object FrmMain: TFrmMain
  Left = 238
  Top = 208
  Width = 838
  Height = 568
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
    Left = 153
    Top = 38
    Width = 4
    Height = 503
    Cursor = crHSplit
  end
  object ButtonsPanel: TPanel
    Left = 0
    Top = 0
    Width = 830
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
    Width = 153
    Height = 503
    Align = alLeft
    ItemHeight = 13
    TabOrder = 1
    OnClick = ListBox1Click
  end
  object ListView1: TListView
    Left = 157
    Top = 38
    Width = 673
    Height = 503
    Align = alClient
    Columns = <
      item
        Caption = 'Type'
        Width = 100
      end
      item
        Caption = 'Date'
        Width = 70
      end
      item
        Caption = 'Time'
        Width = 70
      end
      item
        Caption = 'Source'
        Width = 100
      end
      item
        Caption = 'Category'
        Width = 100
      end
      item
        Caption = 'Event'
      end
      item
        Caption = 'User'
        Width = 90
      end
      item
        Caption = 'Computer'
        Width = 90
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
