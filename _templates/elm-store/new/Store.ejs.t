---
inject: false
to: src/<%=Name+"Store"%>.elm

---
<%
  modelId = Name + "Id";
  storeName = Name+"Store";
  modelName = Name;
%>
module <%=storeName%> exposing (<%=storeName%>)

import Dict exposing (Dict)
import Grain exposing (Grain)
import GrainId exposing (GrainId)


type alias Model =
    { lookup : Dict GrainId Grain }


type <%=storeName%>
    = <%=storeName%> Model
