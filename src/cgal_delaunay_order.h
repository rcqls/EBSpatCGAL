#ifndef CGAL_DELAUNAY_ORDER_H
#define CGAL_DELAUNAY_ORDER_H
#include <CGAL/Unique_hash_map.h>
#include <queue>

Delaunay2_VertexSet_Set Delaunay2_order_neighbors_list(Delaunay2 *obj, Del2D_Vertex_handle v, std::size_t k) 
{  
  Delaunay2_VertexSet_Set  res;  
  CGAL::Unique_hash_map<Del2D_Vertex_handle, int>  mark;
  int cur_mark = 1;

  // mark vertex v
  mark[v] = cur_mark;
  std::queue<Del2D_Vertex_handle> del2n;

  del2n.push(v);
      
  while ( k > 0 )
  { 
    Del2D_Vertex_handle w = del2n.front();
    del2n.pop();

    k--; 

    // get the incident vertices of w ...
    Delaunay2::Vertex_circulator vc = obj->incident_vertices(w);
    Delaunay2::Vertex_circulator start =vc;
    Del2D_Vertex_handle act;
     
    do {
         act = vc;
	 
      	 // test, if act is marked ...
      	 bool is_marked = mark.is_defined(act);  
	 
         if ( (! is_marked) && (! obj->is_infinite(act)) ) { 
            Delaunay2_VertexSet e;
            e.insert(w);e.insert(act);
            res.insert(e);
            del2n.push(act);
            mark[act] = cur_mark;

         }	   
	             
         vc++;
       } while (vc != start);   
        
   }
   return res;
} 
#endif //CGAL_DELAUNAY_ORDER_H