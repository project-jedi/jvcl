object FrameDirEditBrowse: TFrameDirEditBrowse
  Left = 0
  Top = 0
  Width = 320
  Height = 59
  TabOrder = 0
  object LblCaption: TLabel
    Left = 8
    Top = 8
    Width = 50
    Height = 13
    Caption = 'LblCaption'
    FocusControl = EditDirectory
  end
  object Bevel: TBevel
    Left = 0
    Top = 0
    Width = 320
    Height = 9
    Align = alTop
    Shape = bsTopLine
  end
  object EditDirectory: TEdit
    Left = 8
    Top = 24
    Width = 279
    Height = 21
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    ReadOnly = True
    TabOrder = 0
    OnEnter = EditDirectoryEnter
  end
  object BtnJCLDirBrowse: TButton
    Left = 288
    Top = 24
    Width = 21
    Height = 21
    Hint = 'Browse...'
    Caption = '...'
    ParentShowHint = False
    ShowHint = True
    TabOrder = 1
    OnClick = BtnJCLDirBrowseClick
  end
end
