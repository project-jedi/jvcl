object frmUnitStatus: TfrmUnitStatus
  Left = 318
  Top = 153
  Width = 533
  Height = 424
  Caption = 'frmUnitStatus'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object ListView1: TListView
    Left = 8
    Top = 8
    Width = 505
    Height = 337
    Columns = <
      item
        Caption = 'Filename'
        Width = 150
      end
      item
        Caption = 'Text'
        Width = 200
      end
      item
        Caption = 'Interpretation'
        Width = 100
      end>
    TabOrder = 0
    ViewStyle = vsReport
    OnColumnClick = ListView1ColumnClick
    OnCompare = ListView1Compare
  end
  object Button1: TButton
    Left = 328
    Top = 352
    Width = 185
    Height = 25
    Caption = 'Update settings'
    TabOrder = 1
    OnClick = Button1Click
  end
  object JvSearchFiles1: TJvSearchFiles
    DirOption = doExcludeSubDirs
    Options = [soOwnerData, soSearchFiles]
    ErrorResponse = erIgnore
    FileParams.SearchTypes = [stFileMask]
    FileParams.FileMasks.Strings = (
      '*.dtx')
    OnFindFile = JvSearchFiles1FindFile
    Left = 312
    Top = 168
  end
end
