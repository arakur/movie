namespace Script

open FSharpPlus
open FSharpPlus.Data
open FSharpx.Collections
open System.Collections.Generic

type PriorityRelationGraphNode =
    { Succ: AssetRef Set
      Pred: AssetRef Set }

    static member empty = { Succ = Set.empty; Pred = Set.empty }

    static member succ this = this.Succ

    static member pred this = this.Pred

    static member addSucc node this =
        { this with
            Succ = this.Succ |> Set.add node }

    static member addPred node this =
        { this with
            Pred = this.Pred |> Set.add node }

    static member removeSucc node this =
        { this with
            Succ = this.Succ |> Set.remove node }

    static member removePred node this =
        { this with
            Pred = this.Pred |> Set.remove node }

type PriorityRelationGraph =
    { NextNodeIndex: int
      NextEdgeIndex: int
      Edges: Map<AssetRef, PriorityRelationGraphNode>
      NodeIndices: Map<AssetRef, int>
      EdgeIndices: Map<AssetRef * AssetRef, int> }

    static member empty =
        { NextNodeIndex = 0
          NextEdgeIndex = 0
          Edges = Map.empty
          NodeIndices = Map.empty
          EdgeIndices = Map.empty }

    member this.Nodes = this.NodeIndices |> Map.keys

    member this.NodeIndex node = this.NodeIndices.[node]

    member this.EdgeIndex edge = this.EdgeIndices.[edge]

    static member addNode node this =
        let edges' = this.Edges |> Map.add node PriorityRelationGraphNode.empty
        let nodeIndices' = this.NodeIndices |> Map.add node this.NextNodeIndex
        let index' = this.NextNodeIndex + 1

        { this with
            Edges = edges'
            NodeIndices = nodeIndices'
            NextNodeIndex = index' }

    static member addEdge lower upper this =
        // Memoized DFS to collect all edges, which is the minimal set of edges, which resolves the conflict in adding lower -> upper,
        // with choosing the minimum one respect to edge indices.

        // Create a cache to memoize the result of DFS.

        let mutable cache = Dictionary()

        cache.Add(lower, (DList.empty, System.Int32.MaxValue))

        let cacheResult current ret =
            cache.[current] <- ret
            ret

        // DFS procedure.

        let rec search current =
            let nbh = this.Edges.[current].Succ

            let unionResults (edge0, minIndex0) (edge1, minIndex1) =
                DList.append edge0 edge1, min minIndex0 minIndex1

            let collectNbh () =
                // REMARK: Appending is in O(1) × #nbh, thus total time complexity for appending is O(E).
                nbh |> Seq.map (collectNext current) |> Seq.reduce unionResults

            cache.TryFind current |> Option.defaultWith (collectNbh >> cacheResult current)

        and collectNext current next =
            let currentEdge = current, next
            let currentIndex = this.EdgeIndex currentEdge

            let children, childrenMinIndex = search next

            if currentIndex < childrenMinIndex then
                DList.singleton currentEdge, currentIndex
            else
                children, childrenMinIndex

        // Collect edges to remove.

        let edgesRemove, _ = search upper

        // Remove edges and add an edge lower -> upper.

        let removeEdge edges (remove0, remove1) =
            edges
            |> Map.updateWith (PriorityRelationGraphNode.removeSucc remove1 >> Some) remove0
            |> Map.updateWith (PriorityRelationGraphNode.removePred remove0 >> Some) remove1

        let edges' =
            (this.Edges, edgesRemove)
            ||> Seq.fold removeEdge
            |> Map.updateWith (PriorityRelationGraphNode.addSucc upper >> Some) lower
            |> Map.updateWith (PriorityRelationGraphNode.addPred lower >> Some) upper

        let edgeIndices' = this.EdgeIndices |> Map.add (lower, upper) this.NextEdgeIndex

        let index' = this.NextEdgeIndex + 1

        { this with
            Edges = edges'
            EdgeIndices = edgeIndices'
            NextEdgeIndex = index' }

    static member sort this =
        let rec collect (used, ret) current =
            if used |> Set.contains current then
                used, ret
            else
                let nbh = this.Edges.[current].Succ

                let used' = used |> Set.add current

                // Collect with iterating over all neighbors from the largest index.
                let nbhDecreasing = nbh |> Seq.sortByDescending this.NodeIndex

                let pushCurrent (used', ret) = used', current :: ret

                ((used', ret), nbhDecreasing) ||> Seq.fold collect |> pushCurrent

        ((Set.empty, []), this.Nodes) ||> Seq.fold collect
