  �-  �   k820309    l          18.0        (V�]                                                                                                          
       Interfaces.f90 MPI__LOGICAL_SV              gen@MPI_GATHER gen@MPI_GATHERV gen@MPI_SCATTER gen@MPI_SCATTERV gen@MPI_ALLGATHER gen@MPI_ALLGATHERV gen@MPI_ALLTOALL gen@MPI_ALLTOALLV gen@MPI_REDUCE gen@MPI_ALLREDUCE gen@MPI_REDUCE_SCATTER gen@MPI_SCAN                                                     
       TIMER_MPI                                                        u #MPI_GATHER_T    #         @     @X                                              	   #SENDBUF    #SENDCOUNT    #SENDTYPE    #RECVBUF    #RECVCOUNT    #RECVTYPE    #ROOT 	   #COMM 
   #IERROR              
@ @                                                    
@ @                                                    
@ @                                                 @  D @                                                       p          1     1                             
@ @                                                    
@ @                                                    
@ @                               	                     
@ @                               
                     D @                                                                                                  u #MPI_GATHERV_T    #         @     @X                                              
   #SENDBUF    #SENDCOUNT    #SENDTYPE    #RECVBUF    #RECVCOUNTS    #DISPLS    #RECVTYPE    #ROOT    #COMM    #IERROR              
@ @                                                    
@ @                                                    
@ @                                                 @  D @                                                       p          1     1                          @  
@ @                                                      p          1     1                          @  
@ @                                                      p          1     1                             
@ @                                                    
@ @                                                    
@ @                                                    D @                                                                                                  u #MPI_SCATTER_T    #         @     @X                                              	   #SENDBUF    #SENDCOUNT    #SENDTYPE    #RECVBUF    #RECVCOUNT    #RECVTYPE    #ROOT    #COMM    #IERROR               
@ @                                                    
@ @                                                    
@ @                                                 @  D @                                                       p          1     1                             
@ @                                                    
@ @                                                    
@ @                                                    
@ @                                                    D @                                                                                                   u #MPI_SCATTERV_T !   #         @     @X                             !                 
   #SENDBUF "   #SENDCOUNTS #   #DISPLS $   #SENDTYPE %   #RECVBUF &   #RECVCOUNT '   #RECVTYPE (   #ROOT )   #COMM *   #IERROR +             
@ @                               "                  @  
@ @                              #                        p          1     1                          @  
@ @                              $                     	   p          1     1                             
@ @                               %                  @  D @                              &                     
    p          1     1                             
@ @                               '                     
@ @                               (                     
@ @                               )                     
@ @                               *                     D @                               +                                                                   u #MPI_ALLGATHER_T ,   #         @     @X                             ,                    #SENDBUF -   #SENDCOUNT .   #SENDTYPE /   #RECVBUF 0   #RECVCOUNT 1   #RECVTYPE 2   #COMM 3   #IERROR 4             
@ @                               -                     
@ @                               .                     
@ @                               /                  @  D @                              0                         p          1     1                             
@ @                               1                     
@ @                               2                     
@ @                               3                     D @                               4                                                                   u #MPI_ALLGATHERV_T 5   #         @     @X                             5                 	   #SENDBUF 6   #SENDCOUNT 7   #SENDTYPE 8   #RECVBUF 9   #RECVCOUNTS :   #DISPLS ;   #RECVTYPE <   #COMM =   #IERROR >             
@ @                               6                     
@ @                               7                     
@ @                               8                  @  D @                              9                         p          1     1                          @  
@ @                              :                        p          1     1                          @  
@ @                              ;                        p          1     1                             
@ @                               <                     
@ @                               =                     D @                               >                                                                   u #MPI_ALLTOALL_T ?   #         @     @X                             ?                    #SENDBUF @   #SENDCOUNT A   #SENDTYPE B   #RECVBUF C   #RECVCOUNT D   #RECVTYPE E   #COMM F   #IERROR G             
@ @                               @                     
@ @                               A                     
@ @                               B                  @  D @                              C                         p          1     1                             
@ @                               D                     
@ @                               E                     
@ @                               F                     D @                               G                                                                   u #MPI_ALLTOALLV_T H   #         @     @X                             H                 
   #SENDBUF I   #SENDCOUNTS J   #SDISPLS K   #SENDTYPE L   #RECVBUF M   #RECVCOUNTS N   #RDISPLS O   #RECVTYPE P   #COMM Q   #IERROR R             
@ @                               I                  @  
@ @                              J                        p          1     1                          @  
@ @                              K                        p          1     1                             
@ @                               L                  @  D @                              M                         p          1     1                          @  
@ @                              N                        p          1     1                          @  
@ @                              O                        p          1     1                             
@ @                               P                     
@ @                               Q                     D @                               R                                                                   u #MPI_REDUCE_T S   #         @     @X                             S                    #SENDBUF T   #RECVBUF U   #COUNT V   #DATATYPE W   #OP X   #ROOT Y   #COMM Z   #IERROR [             
@ @                               T                  @  D @                              U                         p          1     1                             
@ @                               V                     
@ @                               W                     
@ @                               X                     
@ @                               Y                     
@ @                               Z                     D @                               [                                                                   u #MPI_ALLREDUCE_T \   #         @     @X                             \                    #SENDBUF ]   #RECVBUF ^   #COUNT _   #DATATYPE `   #OP a   #COMM b   #IERROR c             
@ @                               ]                  @  D @                              ^                         p          1     1                             
@ @                               _                     
@ @                               `                     
@ @                               a                     
@ @                               b                     D @                               c                                                                   u #MPI_REDUCE_SCATTER_T d   #         @     @X                             d                    #SENDBUF e   #RECVBUF f   #RECVCOUNTS g   #DATATYPE h   #OP i   #COMM j   #IERROR k             
@ @                               e                  @  D @                              f                         p          1     1                          @  
@ @                              g                        p          1     1                             
@ @                               h                     
@ @                               i                     
@ @                               j                     D @                               k                                                                   u #MPI_SCAN_T l   #         @     @X                             l                    #SENDBUF m   #RECVBUF n   #COUNT o   #DATATYPE p   #OP q   #COMM r   #IERROR s             
@ @                               m                  @  D @                              n                         p          1     1                             
@ @                               o                     
@ @                               p                     
@ @                               q                     
@ @                               r                     D @                               s               �   '      fn#fn %   �   �   b   uapp(MPI__LOGICAL_SV    �  J   J  TIMER_MPI_M    �  R       gen@MPI_GATHER    @  �      MPI_GATHER_T %   �  @   a   MPI_GATHER_T%SENDBUF '   <  @   a   MPI_GATHER_T%SENDCOUNT &   |  @   a   MPI_GATHER_T%SENDTYPE %   �  �   a   MPI_GATHER_T%RECVBUF '   @  @   a   MPI_GATHER_T%RECVCOUNT &   �  @   a   MPI_GATHER_T%RECVTYPE "   �  @   a   MPI_GATHER_T%ROOT "      @   a   MPI_GATHER_T%COMM $   @  @   a   MPI_GATHER_T%IERROR     �  S       gen@MPI_GATHERV    �  �      MPI_GATHERV_T &   �  @   a   MPI_GATHERV_T%SENDBUF (   �  @   a   MPI_GATHERV_T%SENDCOUNT '     @   a   MPI_GATHERV_T%SENDTYPE &   \  �   a   MPI_GATHERV_T%RECVBUF )   �  �   a   MPI_GATHERV_T%RECVCOUNTS %   d  �   a   MPI_GATHERV_T%DISPLS '   �  @   a   MPI_GATHERV_T%RECVTYPE #   (	  @   a   MPI_GATHERV_T%ROOT #   h	  @   a   MPI_GATHERV_T%COMM %   �	  @   a   MPI_GATHERV_T%IERROR     �	  S       gen@MPI_SCATTER    ;
  �      MPI_SCATTER_T &   �
  @   a   MPI_SCATTER_T%SENDBUF (   7  @   a   MPI_SCATTER_T%SENDCOUNT '   w  @   a   MPI_SCATTER_T%SENDTYPE &   �  �   a   MPI_SCATTER_T%RECVBUF (   ;  @   a   MPI_SCATTER_T%RECVCOUNT '   {  @   a   MPI_SCATTER_T%RECVTYPE #   �  @   a   MPI_SCATTER_T%ROOT #   �  @   a   MPI_SCATTER_T%COMM %   ;  @   a   MPI_SCATTER_T%IERROR !   {  T       gen@MPI_SCATTERV    �  �      MPI_SCATTERV_T '   �  @   a   MPI_SCATTERV_T%SENDBUF *   �  �   a   MPI_SCATTERV_T%SENDCOUNTS &   \  �   a   MPI_SCATTERV_T%DISPLS (   �  @   a   MPI_SCATTERV_T%SENDTYPE '      �   a   MPI_SCATTERV_T%RECVBUF )   �  @   a   MPI_SCATTERV_T%RECVCOUNT (   �  @   a   MPI_SCATTERV_T%RECVTYPE $   $  @   a   MPI_SCATTERV_T%ROOT $   d  @   a   MPI_SCATTERV_T%COMM &   �  @   a   MPI_SCATTERV_T%IERROR "   �  U       gen@MPI_ALLGATHER     9  �      MPI_ALLGATHER_T (   �  @   a   MPI_ALLGATHER_T%SENDBUF *   +  @   a   MPI_ALLGATHER_T%SENDCOUNT )   k  @   a   MPI_ALLGATHER_T%SENDTYPE (   �  �   a   MPI_ALLGATHER_T%RECVBUF *   /  @   a   MPI_ALLGATHER_T%RECVCOUNT )   o  @   a   MPI_ALLGATHER_T%RECVTYPE %   �  @   a   MPI_ALLGATHER_T%COMM '   �  @   a   MPI_ALLGATHER_T%IERROR #   /  V       gen@MPI_ALLGATHERV !   �  �      MPI_ALLGATHERV_T )   D  @   a   MPI_ALLGATHERV_T%SENDBUF +   �  @   a   MPI_ALLGATHERV_T%SENDCOUNT *   �  @   a   MPI_ALLGATHERV_T%SENDTYPE )     �   a   MPI_ALLGATHERV_T%RECVBUF ,   �  �   a   MPI_ALLGATHERV_T%RECVCOUNTS (     �   a   MPI_ALLGATHERV_T%DISPLS *   �  @   a   MPI_ALLGATHERV_T%RECVTYPE &   �  @   a   MPI_ALLGATHERV_T%COMM (     @   a   MPI_ALLGATHERV_T%IERROR !   P  T       gen@MPI_ALLTOALL    �  �      MPI_ALLTOALL_T '   V  @   a   MPI_ALLTOALL_T%SENDBUF )   �  @   a   MPI_ALLTOALL_T%SENDCOUNT (   �  @   a   MPI_ALLTOALL_T%SENDTYPE '     �   a   MPI_ALLTOALL_T%RECVBUF )   �  @   a   MPI_ALLTOALL_T%RECVCOUNT (   �  @   a   MPI_ALLTOALL_T%RECVTYPE $     @   a   MPI_ALLTOALL_T%COMM &   Z  @   a   MPI_ALLTOALL_T%IERROR "   �  U       gen@MPI_ALLTOALLV     �  �      MPI_ALLTOALLV_T (   �  @   a   MPI_ALLTOALLV_T%SENDBUF +   �  �   a   MPI_ALLTOALLV_T%SENDCOUNTS (   �  �   a   MPI_ALLTOALLV_T%SDISPLS )     @   a   MPI_ALLTOALLV_T%SENDTYPE (   E  �   a   MPI_ALLTOALLV_T%RECVBUF +   �  �   a   MPI_ALLTOALLV_T%RECVCOUNTS (   M   �   a   MPI_ALLTOALLV_T%RDISPLS )   �   @   a   MPI_ALLTOALLV_T%RECVTYPE %   !  @   a   MPI_ALLTOALLV_T%COMM '   Q!  @   a   MPI_ALLTOALLV_T%IERROR    �!  R       gen@MPI_REDUCE    �!  �      MPI_REDUCE_T %   �"  @   a   MPI_REDUCE_T%SENDBUF %   �"  �   a   MPI_REDUCE_T%RECVBUF #   J#  @   a   MPI_REDUCE_T%COUNT &   �#  @   a   MPI_REDUCE_T%DATATYPE     �#  @   a   MPI_REDUCE_T%OP "   
$  @   a   MPI_REDUCE_T%ROOT "   J$  @   a   MPI_REDUCE_T%COMM $   �$  @   a   MPI_REDUCE_T%IERROR "   �$  U       gen@MPI_ALLREDUCE     %  �      MPI_ALLREDUCE_T (   �%  @   a   MPI_ALLREDUCE_T%SENDBUF (   �%  �   a   MPI_ALLREDUCE_T%RECVBUF &   |&  @   a   MPI_ALLREDUCE_T%COUNT )   �&  @   a   MPI_ALLREDUCE_T%DATATYPE #   �&  @   a   MPI_ALLREDUCE_T%OP %   <'  @   a   MPI_ALLREDUCE_T%COMM '   |'  @   a   MPI_ALLREDUCE_T%IERROR '   �'  Z       gen@MPI_REDUCE_SCATTER %   (  �      MPI_REDUCE_SCATTER_T -   �(  @   a   MPI_REDUCE_SCATTER_T%SENDBUF -   �(  �   a   MPI_REDUCE_SCATTER_T%RECVBUF 0   x)  �   a   MPI_REDUCE_SCATTER_T%RECVCOUNTS .   �)  @   a   MPI_REDUCE_SCATTER_T%DATATYPE (   <*  @   a   MPI_REDUCE_SCATTER_T%OP *   |*  @   a   MPI_REDUCE_SCATTER_T%COMM ,   �*  @   a   MPI_REDUCE_SCATTER_T%IERROR    �*  P       gen@MPI_SCAN    L+  �      MPI_SCAN_T #   �+  @   a   MPI_SCAN_T%SENDBUF #   %,  �   a   MPI_SCAN_T%RECVBUF !   �,  @   a   MPI_SCAN_T%COUNT $   �,  @   a   MPI_SCAN_T%DATATYPE    )-  @   a   MPI_SCAN_T%OP     i-  @   a   MPI_SCAN_T%COMM "   �-  @   a   MPI_SCAN_T%IERROR 