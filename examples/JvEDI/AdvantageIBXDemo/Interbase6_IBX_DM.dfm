object Interbase6_IBX_Data: TInterbase6_IBX_Data
  OldCreateOrder = False
  OnCreate = DataModuleCreate
  OnDestroy = DataModuleDestroy
  Left = 343
  Top = 250
  Height = 212
  Width = 234
  object JvEDIDBBuffer: TJvEDIDBBuffer
    ElementProfiles = EProfile
    SegmentProfiles = SProfile
    LoopProfiles = LProfile
    KeySuffix = '_Id'
    LoopKeyPrefix = 'Loop_'
    ElementNonKeyPrefix = 'E'
    OnBeforeOpenDataSets = JvEDIDBBufferBeforeOpenDataSets
    OnAfterOpenDataSets = JvEDIDBBufferAfterOpenDataSets
    OnAfterCloseDataSets = JvEDIDBBufferAfterCloseDataSets
    OnTableExists = JvEDIDBBufferTableExists
    OnCreateTable = JvEDIDBBufferCreateTable
    OnCheckForFieldChanges = JvEDIDBBufferCheckForFieldChanges
    OnAlterTable = JvEDIDBBufferAlterTable
    OnBeforeApplyElementFilter = JvEDIDBBufferBeforeApplyElementFilter
    Left = 136
    Top = 16
  end
  object ibdDB: TIBDatabase
    DatabaseName = 
      'C:\RaysDelphiJediProjects\EDI\EDISource\Programs\DB_Interbase6\E' +
      'DISDKDB.GDB'
    Params.Strings = (
      'USER_NAME=SYSDBA'
      'PASSWORD=masterkey')
    LoginPrompt = False
    DefaultTransaction = ibdTrans
    Left = 20
    Top = 16
  end
  object ibdTrans: TIBTransaction
    DefaultDatabase = ibdDB
    DefaultAction = TARollback
    Left = 72
    Top = 16
  end
  object EProfile: TIBDataSet
    Database = ibdDB
    Transaction = ibdTrans
    DeleteSQL.Strings = (
      'delete from EDI_ELEMENT_PROFILE'
      'where'
      '  ELEMENTID = :OLD_ELEMENTID and'
      '  SEGMENTID = :OLD_SEGMENTID')
    InsertSQL.Strings = (
      'insert into EDI_ELEMENT_PROFILE'
      
        '  (ELEMENTCOUNT, ELEMENTID, ELEMENTTYPE, MAXIMUMLENGTH, SEGMENTI' +
        'D)'
      'values'
      
        '  (:ELEMENTCOUNT, :ELEMENTID, :ELEMENTTYPE, :MAXIMUMLENGTH, :SEG' +
        'MENTID)')
    RefreshSQL.Strings = (
      'Select '
      '  SEGMENTID,'
      '  ELEMENTID,'
      '  ELEMENTCOUNT,'
      '  ELEMENTTYPE,'
      '  MAXIMUMLENGTH'
      'from EDI_ELEMENT_PROFILE '
      'where'
      '  ELEMENTID = :ELEMENTID and'
      '  SEGMENTID = :SEGMENTID')
    SelectSQL.Strings = (
      'select * from EDI_ELEMENT_PROFILE')
    ModifySQL.Strings = (
      'update EDI_ELEMENT_PROFILE'
      'set'
      '  ELEMENTCOUNT = :ELEMENTCOUNT,'
      '  ELEMENTID = :ELEMENTID,'
      '  ELEMENTTYPE = :ELEMENTTYPE,'
      '  MAXIMUMLENGTH = :MAXIMUMLENGTH,'
      '  SEGMENTID = :SEGMENTID'
      'where'
      '  ELEMENTID = :OLD_ELEMENTID and'
      '  SEGMENTID = :OLD_SEGMENTID')
    Left = 20
    Top = 64
  end
  object SProfile: TIBDataSet
    Database = ibdDB
    Transaction = ibdTrans
    DeleteSQL.Strings = (
      'delete from EDI_SEGMENT_PROFILE'
      'where'
      '  OWNERLOOPID = :OLD_OWNERLOOPID and'
      '  PARENTLOOPID = :OLD_PARENTLOOPID and'
      '  SEGMENTID = :OLD_SEGMENTID')
    InsertSQL.Strings = (
      'insert into EDI_SEGMENT_PROFILE'
      '  (OWNERLOOPID, PARENTLOOPID, SEGMENTID)'
      'values'
      '  (:OWNERLOOPID, :PARENTLOOPID, :SEGMENTID)')
    RefreshSQL.Strings = (
      'Select '
      '  SEGMENTID,'
      '  OWNERLOOPID,'
      '  PARENTLOOPID'
      'from EDI_SEGMENT_PROFILE '
      'where'
      '  OWNERLOOPID = :OWNERLOOPID and'
      '  PARENTLOOPID = :PARENTLOOPID and'
      '  SEGMENTID = :SEGMENTID')
    SelectSQL.Strings = (
      'select * from EDI_SEGMENT_PROFILE')
    ModifySQL.Strings = (
      'update EDI_SEGMENT_PROFILE'
      'set'
      '  OWNERLOOPID = :OWNERLOOPID,'
      '  PARENTLOOPID = :PARENTLOOPID,'
      '  SEGMENTID = :SEGMENTID'
      'where'
      '  OWNERLOOPID = :OLD_OWNERLOOPID and'
      '  PARENTLOOPID = :OLD_PARENTLOOPID and'
      '  SEGMENTID = :OLD_SEGMENTID')
    Left = 64
    Top = 64
  end
  object LProfile: TIBDataSet
    Database = ibdDB
    Transaction = ibdTrans
    DeleteSQL.Strings = (
      'delete from EDI_LOOP_PROFILE'
      'where'
      '  OWNERLOOPID = :OLD_OWNERLOOPID and'
      '  PARENTLOOPID = :OLD_PARENTLOOPID')
    InsertSQL.Strings = (
      'insert into EDI_LOOP_PROFILE'
      '  (OWNERLOOPID, PARENTLOOPID)'
      'values'
      '  (:OWNERLOOPID, :PARENTLOOPID)')
    RefreshSQL.Strings = (
      'Select '
      '  OWNERLOOPID,'
      '  PARENTLOOPID'
      'from EDI_LOOP_PROFILE '
      'where'
      '  OWNERLOOPID = :OWNERLOOPID and'
      '  PARENTLOOPID = :PARENTLOOPID')
    SelectSQL.Strings = (
      'select * from EDI_LOOP_PROFILE')
    ModifySQL.Strings = (
      'update EDI_LOOP_PROFILE'
      'set'
      '  OWNERLOOPID = :OWNERLOOPID,'
      '  PARENTLOOPID = :PARENTLOOPID'
      'where'
      '  OWNERLOOPID = :OLD_OWNERLOOPID and'
      '  PARENTLOOPID = :OLD_PARENTLOOPID')
    Left = 108
    Top = 64
  end
  object dsEProfile: TDataSource
    DataSet = EProfile
    Left = 20
    Top = 108
  end
  object dsSProfile: TDataSource
    DataSet = SProfile
    Left = 64
    Top = 108
  end
  object dsLProfile: TDataSource
    DataSet = LProfile
    Left = 108
    Top = 108
  end
  object IBTable: TIBTable
    Database = ibdDB
    Transaction = ibdTrans
    Left = 168
    Top = 72
  end
  object IBQuery: TIBQuery
    Database = ibdDB
    Transaction = ibdTrans
    Left = 176
    Top = 124
  end
end
