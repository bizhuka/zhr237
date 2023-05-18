interface ZIF_C_HR237_LAYER_C
  public .


  interfaces /BOBF/IF_LIB_CONSTANTS .

  constants:
    BEGIN OF SC_ACTION,
      BEGIN OF ZC_HR237_LAYER,
 CREATE_ZC_HR237_LAYER          TYPE /BOBF/ACT_KEY VALUE 'E9DB6CE3AAD91EDDBBFE5F3DA4B74BA6',
 DELETE_ZC_HR237_LAYER          TYPE /BOBF/ACT_KEY VALUE 'E9DB6CE3AAD91EDDBBFE5F3DA4B7CBA6',
 LOCK_ZC_HR237_LAYER            TYPE /BOBF/ACT_KEY VALUE 'E9DB6CE3AAD91EDDBBFE5F3DA4B68BA6',
 SAVE_ZC_HR237_LAYER            TYPE /BOBF/ACT_KEY VALUE 'E9DB6CE3AAD91EDDBBFE5F3DA4B84BA6',
 UNLOCK_ZC_HR237_LAYER          TYPE /BOBF/ACT_KEY VALUE 'E9DB6CE3AAD91EDDBBFE5F3DA4B6CBA6',
 UPDATE_ZC_HR237_LAYER          TYPE /BOBF/ACT_KEY VALUE 'E9DB6CE3AAD91EDDBBFE5F3DA4B78BA6',
 VALIDATE_ZC_HR237_LAYER        TYPE /BOBF/ACT_KEY VALUE 'E9DB6CE3AAD91EDDBBFE5F3DA4B80BA6',
      END OF ZC_HR237_LAYER,
    END OF SC_ACTION .
  constants:
    BEGIN OF SC_ACTION_ATTRIBUTE,
        BEGIN OF ZC_HR237_LAYER,
        BEGIN OF LOCK_ZC_HR237_LAYER,
 GENERIC                        TYPE STRING VALUE 'GENERIC',
 EDIT_MODE                      TYPE STRING VALUE 'EDIT_MODE',
 ALL_NONE                       TYPE STRING VALUE 'ALL_NONE',
 SCOPE                          TYPE STRING VALUE 'SCOPE',
 FORCE_INVALIDATION             TYPE STRING VALUE 'FORCE_INVALIDATION',
 LOCK_PARAMETER_BUFFER          TYPE STRING VALUE 'LOCK_PARAMETER_BUFFER',
        END OF LOCK_ZC_HR237_LAYER,
        BEGIN OF UNLOCK_ZC_HR237_LAYER,
 GENERIC                        TYPE STRING VALUE 'GENERIC',
 EDIT_MODE                      TYPE STRING VALUE 'EDIT_MODE',
 ALL_NONE                       TYPE STRING VALUE 'ALL_NONE',
 SCOPE                          TYPE STRING VALUE 'SCOPE',
 FORCE_INVALIDATION             TYPE STRING VALUE 'FORCE_INVALIDATION',
 LOCK_PARAMETER_BUFFER          TYPE STRING VALUE 'LOCK_PARAMETER_BUFFER',
        END OF UNLOCK_ZC_HR237_LAYER,
      END OF ZC_HR237_LAYER,
    END OF SC_ACTION_ATTRIBUTE .
  constants:
    BEGIN OF SC_ALTERNATIVE_KEY,
      BEGIN OF ZC_HR237_LAYER,
 DB_KEY                         TYPE /BOBF/OBM_ALTKEY_KEY VALUE 'E9DB6CE3AAD91EDDBBFE5F3DA4B92BA6',
      END OF ZC_HR237_LAYER,
    END OF SC_ALTERNATIVE_KEY .
  constants:
    BEGIN OF SC_ASSOCIATION,
      BEGIN OF ZC_HR237_LAYER,
 LOCK                           TYPE /BOBF/OBM_ASSOC_KEY VALUE 'E9DB6CE3AAD91EDDBBFE5F3DA4B66BA6',
 MESSAGE                        TYPE /BOBF/OBM_ASSOC_KEY VALUE 'E9DB6CE3AAD91EDDBBFE5F3DA4B62BA6',
 PROPERTY                       TYPE /BOBF/OBM_ASSOC_KEY VALUE 'E9DB6CE3AAD91EDDBBFE5F3DA4B72BA6',
      END OF ZC_HR237_LAYER,
      BEGIN OF ZC_HR237_LAYER_LOCK,
 TO_PARENT                      TYPE /BOBF/OBM_ASSOC_KEY VALUE 'E9DB6CE3AAD91EDDBBFE5F3DA4B8ABA6',
      END OF ZC_HR237_LAYER_LOCK,
      BEGIN OF ZC_HR237_LAYER_MESSAGE,
 TO_PARENT                      TYPE /BOBF/OBM_ASSOC_KEY VALUE 'E9DB6CE3AAD91EDDBBFE5F3DA4B88BA6',
      END OF ZC_HR237_LAYER_MESSAGE,
      BEGIN OF ZC_HR237_LAYER_PROPERTY,
 TO_PARENT                      TYPE /BOBF/OBM_ASSOC_KEY VALUE 'E9DB6CE3AAD91EDDBBFE5F3DA4B8CBA6',
      END OF ZC_HR237_LAYER_PROPERTY,
    END OF SC_ASSOCIATION .
  constants:
    BEGIN OF SC_ASSOCIATION_ATTRIBUTE,
      BEGIN OF ZC_HR237_LAYER,
        BEGIN OF PROPERTY,
 ALL_NODE_PROPERTY              TYPE STRING VALUE 'ALL_NODE_PROPERTY',
 ALL_NODE_ATTRIBUTE_PROPERTY    TYPE STRING VALUE 'ALL_NODE_ATTRIBUTE_PROPERTY',
 ALL_ASSOCIATION_PROPERTY       TYPE STRING VALUE 'ALL_ASSOCIATION_PROPERTY',
 ALL_ASSOCIATION_ATTRIBUTE_PROP TYPE STRING VALUE 'ALL_ASSOCIATION_ATTRIBUTE_PROP',
 ALL_ACTION_PROPERTY            TYPE STRING VALUE 'ALL_ACTION_PROPERTY',
 ALL_ACTION_ATTRIBUTE_PROPERTY  TYPE STRING VALUE 'ALL_ACTION_ATTRIBUTE_PROPERTY',
 ALL_QUERY_PROPERTY             TYPE STRING VALUE 'ALL_QUERY_PROPERTY',
 ALL_QUERY_ATTRIBUTE_PROPERTY   TYPE STRING VALUE 'ALL_QUERY_ATTRIBUTE_PROPERTY',
 ALL_SUBTREE_PROPERTY           TYPE STRING VALUE 'ALL_SUBTREE_PROPERTY',
        END OF PROPERTY,
      END OF ZC_HR237_LAYER,
    END OF SC_ASSOCIATION_ATTRIBUTE .
  constants:
    SC_BO_KEY  TYPE /BOBF/OBM_BO_KEY VALUE 'E9DB6CE3AAD91EDDBBFE5F3DA4B58BA6' .
  constants:
    SC_BO_NAME TYPE /BOBF/OBM_NAME VALUE 'ZC_HR237_LAYER' .
  constants:
    SC_MODEL_VERSION TYPE /BOBF/CONF_VERSION VALUE '00000' .
  constants:
    BEGIN OF SC_NODE,
 ZC_HR237_LAYER                 TYPE /BOBF/OBM_NODE_KEY VALUE 'E9DB6CE3AAD91EDDBBFE5F3DA4B5CBA6',
 ZC_HR237_LAYER_LOCK            TYPE /BOBF/OBM_NODE_KEY VALUE 'E9DB6CE3AAD91EDDBBFE5F3DA4B64BA6',
 ZC_HR237_LAYER_MESSAGE         TYPE /BOBF/OBM_NODE_KEY VALUE 'E9DB6CE3AAD91EDDBBFE5F3DA4B60BA6',
 ZC_HR237_LAYER_PROPERTY        TYPE /BOBF/OBM_NODE_KEY VALUE 'E9DB6CE3AAD91EDDBBFE5F3DA4B70BA6',
    END OF SC_NODE .
  constants:
    BEGIN OF SC_NODE_ATTRIBUTE,
      BEGIN OF ZC_HR237_LAYER,
  NODE_DATA                      TYPE STRING VALUE 'NODE_DATA',
  LAYER_ID                       TYPE STRING VALUE 'LAYER_ID',
  LAYER_TEXT                     TYPE STRING VALUE 'LAYER_TEXT',
  LAYER_ADDRESS                  TYPE STRING VALUE 'LAYER_ADDRESS',
  PERSA                          TYPE STRING VALUE 'PERSA',
  LAYER_IMAGE                    TYPE STRING VALUE 'LAYER_IMAGE',
      END OF ZC_HR237_LAYER,
    END OF SC_NODE_ATTRIBUTE .
  constants:
    BEGIN OF SC_NODE_CATEGORY,
      BEGIN OF ZC_HR237_LAYER,
 ROOT                           TYPE /BOBF/OBM_NODE_CAT_KEY VALUE 'E9DB6CE3AAD91EDDBBFE5F3DA4B5EBA6',
      END OF ZC_HR237_LAYER,
    END OF SC_NODE_CATEGORY .
endinterface.