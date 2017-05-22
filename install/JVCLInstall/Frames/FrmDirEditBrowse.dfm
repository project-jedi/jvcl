object FrameDirEditBrowse: TFrameDirEditBrowse
  Left = 0
  Top = 0
  Width = 320
  Height = 59
  TabOrder = 0
  TabStop = True
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
    Hint = 'Directory does not exist'
    Anchors = [akLeft, akTop, akRight]
    ParentShowHint = False
    ShowHint = False
    TabOrder = 0
    OnChange = EditDirectoryChange
    OnDblClick = EditDirectoryDblClick
    OnExit = EditDirectoryExit
  end
  object BtnDirBrowse: TButton
    Left = 288
    Top = 24
    Width = 21
    Height = 21
    Hint = 'Browse...'
    Anchors = [akLeft, akTop, akRight]
    Caption = '...'
    ParentShowHint = False
    ShowHint = True
    TabOrder = 1
    OnClick = BtnDirBrowseClick
  end
end
